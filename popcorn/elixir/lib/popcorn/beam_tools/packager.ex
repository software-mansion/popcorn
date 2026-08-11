defmodule Popcorn.BeamTools.Packager do
  alias Popcorn.BeamTools.BeamPatcher

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

  # Using `__DIR__` is safe – the plugin is compiled on user's machine
  @patches_dir Path.expand("../../../patches", __DIR__)

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

  @type options :: %{
          root_dir: Path.t(),
          entrypoint_app: String.t() | nil,
          extra_apps: [String.t()],
          out_dir: Path.t(),
          runtimes_dir: Path.t(),
          runtime_variant: String.t() | nil,
          strip: boolean()
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

  @spec run(options()) :: {:ok, map()} | {:error, map()}
  def run(args) do
    %{
      root_dir: root_dir,
      entrypoint_app: entrypoint_app,
      extra_apps: extra_apps,
      out_dir: out_dir,
      runtimes_dir: runtimes_dir,
      runtime_variant: runtime_variant,
      strip: strip
    } = args

    toolchain = fetch_toolchain_info()

    with {:ok, project_apps} <- root_dir |> project_build_dir() |> get_apps_info(),
         {:ok, builtin_apps} <- get_builtin_apps(toolchain),
         {:ok, apps_info} <- apps_to_pack(project_apps, builtin_apps, extra_apps, entrypoint_app),
         variant = runtime_variant || required_runtime(apps_info),
         {:ok, manifest} <- read_manifest(Path.join([runtimes_dir, variant, "manifest.json"])),
         :ok <- check_otp_version(toolchain.otp, manifest.version),
         :ok <- check_capabilities(apps_info, manifest.capabilities),
         {:ok, boot_path} <- create_boot(out_dir, toolchain.otp_root, manifest.preloaded),
         staged_apps = stage_apps(Path.join(out_dir, "staging"), apps_info),
         :ok <- patch_apps(staged_apps) do
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
        toolchain: toolchain
      }

      {:ok, result}
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

  defp project_build_dir(root_dir) do
    build_env = System.get_env("MIX_ENV", "dev")
    build_lib_dir = Path.join([root_dir, "_build", build_env, "lib"])

    build_lib_dir
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

  defp loaded_dynamic_nifs(app, ebin_dir) do
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

  defp encode_json(term) do
    term |> :json.encode() |> IO.iodata_to_binary()
  end

  defp decode_json(json) do
    {:ok, :json.decode(json)}
  rescue
    _ -> :error
  end
end
