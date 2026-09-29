defmodule Popcorn.Packager do
  alias Popcorn.Packager.BeamPatcher

  @static_nif_beams MapSet.new(["wasm.beam", "prim_tty.beam", "zstd.beam"])

  # :beam_lib.significant_chunks/0 is undocumented.
  @retained_chunks :beam_lib.significant_chunks() -- [~c"Type"]
  # Applications that only work when the emulator was built with the matching
  # native support. The runtime manifest declares what the build provides.
  @app_capabilities %{
    "asn1" => "crypto",
    "crypto" => "crypto",
    "public_key" => "crypto",
    "ssl" => "crypto"
  }

  @boot_name "bin/vm.boot"
  @runtime_files ~w(beam.mjs beam.emu.mjs beam.wasm)

  @patches_dir Path.expand("../../patches", __DIR__)

  # To run, Beam needs following apps:
  # - kernel
  # - stdlib (kernel dep)
  #
  # We also add elixir support out of the box and need:
  # - compiler (elixir dep)
  # - elixir
  #
  # All of them should be in elixir's transitive dependency closure
  @base_apps ["elixir"]

  @type options :: [
          {:root_dir, Path.t()}
          | {:build_path, Path.t()}
          | {:out_dir, Path.t()}
          | {:app, String.t() | nil}
          | {:extra_apps, [String.t()]}
          | {:runtime_variant, String.t() | nil}
          | {:brotli, boolean()}
          | {:strip, boolean()}
          | {:treeshake, false | [preserved_apps: [String.t()]]}
          | {:static_dir, Path.t()}
        ]

  @type pack_options :: %{
          build_path: Path.t(),
          entrypoint_app: String.t() | nil,
          extra_apps: [String.t()],
          out_dir: Path.t(),
          runtimes_dir: Path.t(),
          runtime_variant: String.t() | nil,
          strip: boolean(),
          treeshake: false | [preserved_apps: [String.t()]]
        }

  defp strip_tarball(path, out_dir) do
    {:ok, entries} = :erl_tar.extract(to_charlist(path), [:memory])

    stripped =
      entries
      |> Enum.sort_by(fn {name, _content} -> to_string(name) end)
      |> Enum.map(&strip_beam/1)

    output = Path.join(out_dir, Path.basename(path))
    opts = [mtime: 0, atime: 0, ctime: 0, uid: 0, gid: 0]

    :ok = :erl_tar.create(to_charlist(output), stripped, opts)
  end

  defp beam?(path), do: Path.extname(path) == ".beam"

  # Reimplementation of :beam_lib.strip_files/2.
  # Original also gzip compresses modules.
  defp strip_beam({name, content}) do
    if beam?(to_string(name)) do
      {:ok, _module, chunks} = :beam_lib.all_chunks(content)
      chunks_by_name = Map.new(chunks)

      chunks =
        Enum.flat_map(@retained_chunks, fn name ->
          case Map.fetch(chunks_by_name, name) do
            {:ok, data} -> [{name, data}]
            :error -> []
          end
        end)

      {:ok, stripped} = :beam_lib.build_module(chunks)

      {name, stripped}
    else
      {name, content}
    end
  end

  @spec build(options()) :: {:ok, map()} | {:error, map()}
  def build(options) do
    options =
      Keyword.validate!(options,
        root_dir: File.cwd!(),
        build_path: nil,
        out_dir: "priv/static/popcorn",
        app: nil,
        extra_apps: [],
        runtime_variant: nil,
        brotli: false,
        strip: true,
        treeshake: false,
        static_dir: Application.app_dir(:popcorn, "priv/static")
      )

    root_dir = Path.expand(options[:root_dir])

    build_path =
      Path.expand(options[:build_path] || Path.join(Mix.Project.build_path(), "lib"), root_dir)

    out_dir = options |> Keyword.fetch!(:out_dir) |> Path.expand(root_dir)
    static_dir = options |> Keyword.fetch!(:static_dir) |> Path.expand(root_dir)
    packed_dir = Path.join(out_dir, ".packager")

    File.rm_rf!(out_dir)
    File.mkdir_p!(out_dir)

    result =
      with {:ok, report} <-
             pack(%{
               build_path: build_path,
               entrypoint_app: options[:app],
               extra_apps: options[:extra_apps],
               out_dir: packed_dir,
               runtimes_dir: Path.join(static_dir, "runtimes"),
               runtime_variant: options[:runtime_variant],
               strip: options[:strip],
               treeshake: options[:treeshake]
             }),
           {:ok, installed} <-
             install_output(report, static_dir, out_dir, options[:brotli]) do
        {:ok, installed}
      end

    File.rm_rf!(packed_dir)

    case result do
      {:ok, _report} = ok ->
        ok

      {:error, _reason} = error ->
        File.rm_rf!(out_dir)
        error
    end
  end

  @spec pack(pack_options()) :: {:ok, map()} | {:error, map()}
  defp pack(args) do
    %{
      build_path: build_path,
      entrypoint_app: entrypoint_app,
      extra_apps: extra_apps,
      out_dir: out_dir,
      runtimes_dir: runtimes_dir,
      runtime_variant: runtime_variant,
      strip: strip,
      treeshake: treeshake
    } = args

    toolchain = fetch_toolchain_info()

    with {:ok, project_apps} <- get_apps_info(build_path),
         {:ok, builtin_apps} <- get_builtin_apps(toolchain),
         {:ok, apps_info} <- apps_to_pack(project_apps, builtin_apps, extra_apps, entrypoint_app),
         variant = runtime_variant || required_runtime(apps_info),
         {:ok, manifest} <- read_manifest(Path.join([runtimes_dir, variant, "manifest.json"])),
         :ok <- check_otp_version(toolchain.otp, manifest.version),
         :ok <- check_capabilities(apps_info, manifest.capabilities),
         {:ok, boot_path} <- create_boot(out_dir, toolchain.otp_root, manifest.preloaded),
         staged_apps = stage_apps(Path.join(out_dir, "staging"), apps_info),
         :ok <- patch_apps(staged_apps),
         staged_apps = remove_build_tools(staged_apps),
         {:ok, staged_apps, treeshake_report} <-
           maybe_treeshake(staged_apps, treeshake, out_dir) do
      vm_version = manifest.version
      toolchain = Map.take(toolchain, ~w(otp elixir)a)

      File.mkdir_p!(out_dir)

      packed_apps =
        staged_apps
        |> async_stream(fn {app, info} ->
          version = Keyword.get(info.props, :vsn, ~c"") |> to_string()
          tar_path = create_tarball(out_dir, app, info.ebin_dir)

          {app, %{tar: tar_path, version: version}}
        end)
        |> Map.new()

      diagnostics =
        staged_apps
        |> async_stream(fn {app, info} ->
          case loaded_dynamic_nifs(app, info.ebin_dir) do
            [] ->
              []

            beams ->
              {:error, context} = err(:dynamic_nifs_loading, {app, beams})
              [context]
          end
        end)
        |> Enum.concat()

      manifest_path = Path.join(out_dir, "manifest.json")

      packed_tar_paths =
        packed_apps
        |> Map.values()
        |> Enum.map(&Path.expand(Path.join(out_dir, &1.tar)))

      tar_paths = maybe_strip_tarballs(packed_tar_paths, strip, out_dir)

      manifest = %{
        entrypoint: entrypoint_app,
        runtimeVariant: variant,
        apps: packed_apps,
        notes: diagnostics,
        toolchain: toolchain,
        vm: %{boot: @boot_name, version: vm_version}
      }

      File.write!(manifest_path, encode_json(manifest))

      result = %{
        ok: true,
        runtimeVariant: variant,
        entrypoint: entrypoint_app,
        manifestPath: Path.expand(manifest_path),
        bootPath: Path.expand(boot_path),
        tarPaths: tar_paths,
        apps: packed_apps,
        notes: diagnostics,
        toolchain: toolchain,
        treeshake: treeshake_report
      }

      {:ok, result}
    end
  end

  defp install_output(report, static_dir, out_dir, brotli) do
    variant_dir = Path.join([static_dir, "runtimes", report.runtimeVariant])

    js_files = Enum.map(~w(index.mjs worker.mjs), &{&1, Path.join(static_dir, &1)})
    vm_files = Enum.map(@runtime_files, &{&1, Path.join(variant_dir, &1)})

    with :ok <- copy_assets(js_files ++ vm_files, out_dir) do
      manifest_path = Path.join(out_dir, "otp/manifest.json")
      boot_path = Path.join(out_dir, "otp/bin/vm.boot")
      lib_dir = Path.join(out_dir, "otp/lib")

      File.mkdir_p!(Path.dirname(manifest_path))
      File.mkdir_p!(Path.dirname(boot_path))
      File.mkdir_p!(lib_dir)
      File.cp!(report.manifestPath, manifest_path)
      File.cp!(report.bootPath, boot_path)

      tar_paths =
        Enum.map(report.tarPaths, fn source ->
          target = Path.join(lib_dir, Path.basename(source))
          File.cp!(source, target)
          compress(target, brotli)
          Path.expand(target)
        end)

      {:ok,
       %{
         report
         | manifestPath: Path.expand(manifest_path),
           bootPath: Path.expand(boot_path),
           tarPaths: tar_paths
       }}
    end
  end

  defp copy_assets(files, out_dir) do
    outdir_cp = fn {filename, source}, _ ->
      case File.cp(source, Path.join(out_dir, filename)) do
        :ok -> :ok
        {:error, reason} -> err(:missing_runtime_resource, {source, reason})
      end
    end

    case reduce_while_ok(files, :ok, outdir_cp) do
      {:ok, _} -> :ok
      {:error, _reason} = error -> error
    end
  end

  defp compress(path, brotli) do
    contents = File.read!(path)
    File.write!(path <> ".gz", :zlib.gzip(contents))

    if brotli do
      {:ok, compressed} = :brotli.encode(contents, %{quality: 11})
      File.write!(path <> ".br", compressed)
    end
  end

  defp create_boot(out_dir, otp_root, runtime_preloaded) do
    boot_path = Path.join([Path.dirname(otp_root), "bin", "no_dot_erlang.boot"])
    {:script, id, commands} = boot_path |> File.read!() |> :erlang.binary_to_term()

    preloaded =
      Enum.flat_map(commands, fn
        {:preLoaded, modules} -> Enum.map(modules, &to_string/1)
        _ -> []
      end)

    case preloaded -- runtime_preloaded do
      [] ->
        boot = {:script, id, Enum.map(commands, &drop_app_versions/1)}
        path = Path.join(out_dir, @boot_name)

        File.mkdir_p!(Path.dirname(path))

        File.write!(path, :erlang.term_to_binary(boot))

        {:ok, path}

      missing ->
        err(:unsupported_boot, {boot_path, missing})
    end
  end

  # `$ROOT/lib/kernel-10.5/ebin` -> `$ROOT/lib/kernel/ebin`
  defp drop_app_versions({:path, dirs}) do
    {:path, Enum.map(dirs, &drop_version_fragment/1)}
  end

  defp drop_app_versions(command), do: command

  defp drop_version_fragment(dir) do
    [root, "lib", app_version, "ebin"] = dir |> to_string() |> Path.split()
    [app, _version] = String.split(app_version, "-", parts: 2)

    to_charlist(Path.join([root, "lib", app, "ebin"]))
  end

  # Apps are copied out of the host installation so packing can modify them.
  defp stage_apps(staging_dir, apps_info) do
    apps_info
    |> async_stream(fn {app, info} ->
      ebin_dir = Path.join([staging_dir, app, "ebin"])

      File.mkdir_p!(Path.dirname(ebin_dir))
      File.cp_r!(info.ebin_dir, ebin_dir)

      {app, %{info | ebin_dir: ebin_dir}}
    end)
    |> Map.new()
  end

  # we patch only selected modules from OTP, see patches/
  defp patch_apps(staged_apps) do
    [@patches_dir, "*", "*.erl"]
    |> Path.join()
    |> Path.wildcard()
    |> reduce_while_ok(:ok, fn patch_path, :ok ->
      app = patch_path |> Path.dirname() |> Path.basename()
      name = Path.basename(patch_path, ".erl") <> ".beam"
      ebin_dir = Map.fetch!(staged_apps, app).ebin_dir
      beam_path = Path.join(ebin_dir, name)

      BeamPatcher.patch_beam(beam_path, patch_path)
    end)
    |> case do
      {:ok, _} -> :ok
      {:error, _} = error -> error
    end
  end

  defp remove_build_tools(staged_apps) do
    case Map.fetch(staged_apps, "popcorn") do
      :error ->
        staged_apps

      {:ok, info} ->
        {build_tools, runtime_modules} =
          info.props
          |> Keyword.fetch!(:modules)
          |> Enum.split_with(&build_tool?/1)

        Enum.each(build_tools, fn module ->
          File.rm!(Path.join(info.ebin_dir, "#{module}.beam"))
        end)

        replace_app_modules(staged_apps, %{"popcorn" => runtime_modules})
    end
  end

  defp build_tool?(module) do
    name = Atom.to_string(module)

    String.starts_with?(name <> ".", [
      "Elixir.Popcorn.Packager.",
      "Elixir.Treeshake.",
      "treeshake_helper.",
      "Elixir.Mix.Tasks.Popcorn."
    ])
  end

  defp async_stream(enumerable, fun) do
    enumerable
    |> Task.async_stream(fun, timeout: :infinity)
    |> Enum.map(fn {:ok, result} -> result end)
  end

  defp maybe_strip_tarballs(paths, false, _out_dir), do: paths

  defp maybe_strip_tarballs(paths, true, out_dir) do
    Enum.map(paths, fn path ->
      :ok = strip_tarball(path, out_dir)
      Path.expand(Path.join(out_dir, Path.basename(path)))
    end)
  end

  defp get_apps_info(root_dir) do
    extract_info = fn app_path ->
      {:ok, [{:application, name, props}]} = :file.consult(app_path)
      dir = Path.dirname(app_path)

      {to_string(name), %{props: props, ebin_dir: dir, app_path: app_path}}
    end

    get_name = fn {name, _info} -> name end
    get_app_path = fn {_name, info} -> info.app_path end
    duplicated? = fn {_name, paths} -> match?([_, _ | _], paths) end

    apps =
      root_dir
      |> Path.join("*/ebin/*.app")
      |> Path.wildcard()
      |> Enum.map(extract_info)

    duplicates =
      apps
      |> Enum.group_by(get_name, get_app_path)
      |> Enum.filter(duplicated?)
      |> Map.new()

    if Enum.empty?(duplicates) do
      {:ok, Map.new(apps)}
    else
      err(:duplicated_apps, {root_dir, duplicates})
    end
  end

  defp get_builtin_apps(toolchain) do
    with {:ok, elixir_apps} <- get_apps_info(toolchain.elixir_root),
         {:ok, otp_apps} <- get_apps_info(toolchain.otp_root) do
      {:ok, Map.merge(elixir_apps, otp_apps)}
    end
  end

  defp read_manifest(manifest_path) do
    with {:ok, json} <- File.read(manifest_path),
         {:ok, %{"vm" => vm}} <- decode_json(json),
         %{"version" => version, "preloaded" => preloaded, "capabilities" => capabilities} <- vm do
      {:ok, %{version: version, preloaded: preloaded, capabilities: capabilities}}
    else
      _ -> err(:bad_manifest, manifest_path)
    end
  end

  defp apps_to_pack(project_apps, builtin_apps, extra_apps, entrypoint) do
    all_apps_info = Map.merge(builtin_apps, project_apps)

    gather_from_root = fn app, selected ->
      gather_required_apps(all_apps_info, project_apps, app, selected)
    end

    with {:ok, roots} <- root_apps(all_apps_info, extra_apps, entrypoint),
         {:ok, selected_apps} <- reduce_while_ok(roots, MapSet.new(), gather_from_root) do
      all_apps_info
      |> Map.filter(fn {app, _info} -> MapSet.member?(selected_apps, app) end)
      |> Enum.sort()
      |> then(&{:ok, &1})
    end
  end

  defp required_runtime(apps_info) do
    needs_crypto = Enum.any?(apps_info, fn {app, _info} -> @app_capabilities[app] == "crypto" end)
    if needs_crypto, do: "crypto", else: "core"
  end

  defp check_capabilities(apps_info, capabilities) do
    unsupported =
      apps_info
      |> Enum.flat_map(fn {app, _info} ->
        case Map.fetch(@app_capabilities, app) do
          {:ok, capability} -> [%{app: app, capability: capability}]
          :error -> []
        end
      end)
      |> Enum.reject(&Map.fetch!(capabilities, &1.capability))

    if unsupported == [], do: :ok, else: err(:unsupported_apps, unsupported)
  end

  defp validate_preserved_apps(apps_info, treeshake) do
    selected = MapSet.new(apps_info, fn {name, _info} -> name end)

    unknown =
      treeshake
      |> Keyword.fetch!(:preserved_apps)
      |> Enum.reject(&MapSet.member?(selected, &1))
      |> Enum.sort()

    case unknown do
      [] -> :ok
      unknown -> err(:unknown_preserved_apps, unknown)
    end
  end

  defp maybe_treeshake(staged_apps, false, _out_dir), do: {:ok, staged_apps, nil}

  defp maybe_treeshake(staged_apps, options, out_dir) do
    preserved_apps = Keyword.fetch!(options, :preserved_apps)

    with :ok <- validate_preserved_apps(staged_apps, options),
         preserved_modules = preserved_modules(staged_apps, preserved_apps),
         {:ok, stats} <- run_treeshake(staged_apps, preserved_modules, out_dir),
         :ok <- install_treeshaken_beams(staged_apps, preserved_apps, stats.output_dir) do
      report = %{
        preserved_apps: preserved_apps,
        modules_removed: Enum.map(stats.modules_removed, &to_string/1),
        modules_shaken:
          stats.modules_shaked |> Map.keys() |> Enum.map(&to_string/1) |> Enum.sort()
      }

      {:ok, staged_apps, report}
    end
  end

  defp preserved_modules(staged_apps, preserved_apps) do
    Enum.flat_map(preserved_apps, fn app ->
      staged_apps
      |> Map.fetch!(app)
      |> Map.fetch!(:props)
      |> Keyword.fetch!(:modules)
    end)
  end

  defp run_treeshake(staged_apps, preserved_modules, out_dir) do
    files =
      Enum.flat_map(staged_apps, fn {_app, info} ->
        info.ebin_dir |> Path.join("*") |> Path.wildcard()
      end)

    output_dir = Path.join(out_dir, "treeshaken")

    report =
      Treeshake.run(
        ebin_files: files,
        output_dir: output_dir,
        keep: preserved_modules,
        leave: preserved_modules
      )

    {:ok, report}
  rescue
    error -> err(:treeshake_failed, Exception.message(error))
  end

  defp install_treeshaken_beams(staged_apps, preserved_apps, output_dir) do
    preserved = MapSet.new(preserved_apps)

    staged_apps
    |> Enum.reject(fn {app, _info} -> MapSet.member?(preserved, app) end)
    |> replace_beams(output_dir)

    :ok
  end

  defp replace_beams(staged_apps, output_dir) do
    Map.new(staged_apps, fn {app, info} ->
      replacement_dir = info.ebin_dir <> ".replacement"
      File.cp_r!(info.ebin_dir, replacement_dir)

      replacement_beams = replacement_dir |> Path.join("*.beam") |> Path.wildcard()
      Enum.each(replacement_beams, &File.rm!/1)

      surviving =
        Enum.flat_map(replacement_beams, fn replacement ->
          source = Path.join(output_dir, Path.basename(replacement))
          target = Path.join(replacement_dir, Path.basename(replacement))

          case File.cp(source, target) do
            :ok -> [source |> Path.basename(".beam") |> String.to_atom()]
            {:error, :enoent} -> []
          end
        end)

      replacement_info = %{info | ebin_dir: replacement_dir}

      updated_info =
        %{app => replacement_info}
        |> replace_app_modules(%{app => Enum.sort(surviving)})
        |> Map.fetch!(app)

      replace_directory(info.ebin_dir, replacement_dir)

      app_path = Path.join(info.ebin_dir, Path.basename(updated_info.app_path))
      {app, %{updated_info | app_path: app_path, ebin_dir: info.ebin_dir}}
    end)
  end

  defp replace_directory(path, replacement) do
    original = path <> ".original"
    File.rename!(path, original)

    try do
      File.rename!(replacement, path)
    rescue
      error ->
        File.rename!(original, path)
        reraise error, __STACKTRACE__
    end

    File.rm_rf!(original)
  end

  defp replace_app_modules(staged_apps, app_modules) do
    Enum.reduce(app_modules, staged_apps, fn {app, modules}, apps ->
      info = Map.fetch!(apps, app)
      props = Keyword.put(info.props, :modules, modules)
      app_path = Path.join(info.ebin_dir, Path.basename(info.app_path))
      application = {:application, String.to_existing_atom(app), props}

      File.write!(app_path, :io_lib.format(~c"~tp.~n", [application]))
      Map.put(apps, app, %{info | app_path: app_path, props: props})
    end)
  end

  defp root_apps(all_apps_info, extra_apps, entrypoint) do
    with {:ok, roots} <- entrypoint_roots(all_apps_info, entrypoint),
         {:ok, extra} <- extra_roots(all_apps_info, extra_apps) do
      {:ok, extra ++ roots}
    end
  end

  defp entrypoint_roots(_all_apps_info, nil), do: {:ok, @base_apps}

  # The entrypoint may be a builtin app, so a project with no code of its own
  # doesn't need a stub application just to depend on one.
  defp entrypoint_roots(all_apps_info, entrypoint)
       when is_map_key(all_apps_info, entrypoint) do
    {:ok, [entrypoint | @base_apps]}
  end

  defp entrypoint_roots(_all_apps_info, entrypoint),
    do: err(:missing_entrypoint, entrypoint)

  defp extra_roots(all_apps_info, extra_apps) do
    case Enum.reject(extra_apps, &is_map_key(all_apps_info, &1)) do
      [] -> {:ok, extra_apps}
      missing -> err(:missing_extra_apps, Enum.sort(missing))
    end
  end

  defp gather_required_apps(all_apps_info, project_apps, app, selected) do
    if MapSet.member?(selected, app) do
      {:ok, selected}
    else
      info = Map.fetch!(all_apps_info, app)
      selected = MapSet.put(selected, app)

      info.props
      |> get_required_apps()
      |> reduce_while_ok(selected, fn dep, acc ->
        if Map.has_key?(all_apps_info, dep) do
          gather_required_apps(all_apps_info, project_apps, dep, acc)
        else
          project_app_names = Enum.sort(Map.keys(project_apps))
          err(:missing_dep, {app, dep, project_app_names})
        end
      end)
    end
  end

  defp reduce_while_ok(enumerable, acc, f) do
    Enum.reduce_while(enumerable, {:ok, acc}, fn value, {:ok, acc} ->
      case f.(value, acc) do
        :ok -> {:cont, {:ok, acc}}
        {:ok, new_acc} -> {:cont, {:ok, new_acc}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp create_tarball(outdir, app, ebin_dir) do
    tar = "lib/#{app}.tar"
    tar_path = Path.join(outdir, tar)
    tar_path_c = to_charlist(tar_path)
    arc_name = ~c"lib/#{app}/ebin"
    ebin_dir_c = to_charlist(ebin_dir)

    File.mkdir_p!(Path.dirname(tar_path))
    :ok = :erl_tar.create(tar_path_c, [{arc_name, ebin_dir_c}], [])

    tar
  end

  defp loaded_dynamic_nifs(_app, ebin_dir) do
    Path.join(ebin_dir, "*.beam")
    |> Path.wildcard()
    |> Enum.filter(&imports_load_nif?/1)
    |> Enum.map(&Path.basename/1)
    |> Enum.reject(&MapSet.member?(@static_nif_beams, &1))
  end

  defp imports_load_nif?(beam_path) do
    case :beam_lib.chunks(to_charlist(beam_path), [:imports]) do
      {:ok, {_mod, [imports: imports]}} -> {:erlang, :load_nif, 2} in imports
      _ -> false
    end
  end

  defp fetch_toolchain_info() do
    %{
      otp: host_otp_version(),
      elixir: System.version(),
      otp_root: Path.join(to_string(:code.root_dir()), "lib"),
      elixir_root: :elixir |> :code.lib_dir() |> to_string() |> Path.dirname()
    }
  end

  defp check_otp_version(host_version, runtime_version) do
    # host: computer this runs on
    # runtime: vm compiled to wasm
    host = otp_version(host_version)
    runtime = otp_version(runtime_version)

    [host_major | _] = host
    [runtime_major | _] = runtime

    compatible = runtime_major - 2 <= host_major and version_lte?(host, runtime)

    if compatible do
      :ok
    else
      err(:unsupported_otp, {host_version, runtime_version})
    end
  end

  defp host_otp_version do
    path =
      Path.join([to_string(:code.root_dir()), "releases", System.otp_release(), "OTP_VERSION"])

    path
    |> File.read!()
    |> String.trim()
  end

  defp otp_version(version) do
    version
    |> to_string()
    |> String.split("-", parts: 2)
    |> hd()
    |> String.split(".")
    |> Enum.map(&String.to_integer/1)
  end

  defp version_lte?(left, right) do
    width = max(length(left), length(right))
    pad = fn version -> version ++ List.duplicate(0, width - length(version)) end
    pad.(left) <= pad.(right)
  end

  defp get_required_apps(props) do
    prop = &Keyword.get/3
    optional = prop.(props, :optional_applications, []) |> MapSet.new()
    applications = prop.(props, :applications, []) |> MapSet.new()
    included = prop.(props, :included_applications, []) |> MapSet.new()

    MapSet.union(applications, included)
    |> MapSet.difference(optional)
    |> Enum.map(&to_string/1)
  end

  @spec format_error(map()) :: String.t()
  def format_error(%{code: "missing_dep"} = error) do
    "#{error.app} depends on #{error.dep}, which is not available. " <>
      "Project applications: #{Enum.join(error.available_apps, ", ")}."
  end

  def format_error(%{code: "unsupported_apps", apps: apps}) do
    apps = Enum.map_join(apps, ", ", &"#{&1.app} (requires #{&1.capability})")
    "The WASM runtime lacks native support required by: #{apps}."
  end

  def format_error(%{code: "missing_extra_apps", apps: apps}) do
    "Extra applications not found: #{Enum.join(apps, ", ")}."
  end

  def format_error(%{code: "unknown_preserved_apps", apps: apps}) do
    "Preserved applications are not selected for packaging: #{Enum.join(apps, ", ")}."
  end

  def format_error(%{code: "treeshake_failed", message: message}), do: message

  def format_error(%{code: "missing_runtime_resource", path: path}) do
    "Runtime resource not found: #{path}. Build the Popcorn JavaScript artifacts first."
  end

  def format_error(error), do: "Packaging failed: #{inspect(error)}"

  defp err(:missing_entrypoint, app) do
    {:error, %{code: "missing_entrypoint", app: app}}
  end

  defp err(:missing_extra_apps, apps) do
    {:error, %{code: "missing_extra_apps", apps: apps}}
  end

  defp err(:bad_manifest, path) do
    {:error, %{code: "bad_manifest", path: path}}
  end

  defp err(:unsupported_boot, {boot_path, missing_preloaded}) do
    missing = Enum.sort(missing_preloaded)
    {:error, %{code: "unsupported_boot", boot: boot_path, missing_preloaded: missing}}
  end

  defp err(:unsupported_otp, {host, runtime}) do
    {:error, %{code: "unsupported_otp", host: host, runtime: runtime}}
  end

  defp err(:duplicated_apps, {root_dir, duplicates}) do
    {:error, %{code: "duplicated_apps", root_dir: root_dir, duplicates: duplicates}}
  end

  defp err(:unsupported_apps, unsupported) do
    {:error, %{code: "unsupported_apps", apps: Enum.sort_by(unsupported, & &1.app)}}
  end

  defp err(:missing_dep, {app, dep, project_apps}) do
    {:error, %{code: "missing_dep", app: app, dep: dep, available_apps: project_apps}}
  end

  defp err(:dynamic_nifs_loading, {app, beams}) do
    {:error, %{code: "dynamic_nifs_loading", app: app, beams: beams}}
  end

  defp err(:unknown_preserved_apps, apps) do
    {:error, %{code: "unknown_preserved_apps", apps: apps}}
  end

  defp err(:treeshake_failed, message) do
    {:error, %{code: "treeshake_failed", message: message}}
  end

  defp err(:missing_runtime_resource, {path, reason}) do
    {:error, %{code: "missing_runtime_resource", path: path, reason: to_string(reason)}}
  end

  defp encode_json(term) do
    term |> :json.encode() |> IO.iodata_to_binary()
  end

  defp decode_json(json) do
    {:ok, :json.decode(json)}
  rescue
    _ -> :error
  end
end
