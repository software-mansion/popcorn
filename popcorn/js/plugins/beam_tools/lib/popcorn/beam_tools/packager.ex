defmodule Popcorn.BeamTools.Packager do
  @static_nif_beams MapSet.new(["wasm.beam"])

  @type options :: %{
          root_dir: Path.t(),
          entrypoint_app: String.t() | nil,
          out_dir: Path.t(),
          manifest_path: Path.t(),
          strip: boolean(),
          tar_paths: [Path.t()]
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

  defp strip_beam({name, content}) do
    with {:is_beam, true} <- {:is_beam, beam?(to_string(name))},
         {:ok, {_module, stripped_and_compressed}} <- :beam_lib.strip(content) do
      stripped = :zlib.gunzip(stripped_and_compressed)
      {name, stripped}
    else
      {:is_beam, false} -> {name, content}
    end
  end

  @spec run(options()) :: {:ok, map()} | {:error, map()}
  def run(args) do
    %{
      root_dir: root_dir,
      entrypoint_app: entrypoint_app,
      out_dir: out_dir,
      manifest_path: manifest_path,
      strip: strip,
      tar_paths: input_tar_paths
    } = args

    with {:ok, manifest} <- read_manifest(manifest_path),
         {:ok, toolchain} <- fetch_toolchain_info(manifest.version),
         project_apps = root_dir |> project_build_dir() |> get_apps_info(),
         builtin_apps = get_builtin_apps(toolchain),
         {:ok, apps_info} <- apps_to_pack(project_apps, builtin_apps, entrypoint_app) do
      vm_version = manifest.version
      toolchain = Map.take(toolchain, ~w(otp elixir)a)

      File.mkdir_p!(out_dir)

      # TODO: We still use builtin apps, needs to change
      packed_apps =
        apps_info
        |> Task.async_stream(fn {app, info} ->
          version = Keyword.get(info.props, :vsn, ~c"") |> to_string()
          tar_path = create_tarball(out_dir, app, info.ebin_dir)

          {app, %{tar: tar_path, version: version}}
        end)
        |> Map.new(fn {:ok, app} -> app end)

      diagnostics =
        apps_info
        |> Task.async_stream(fn {app, info} ->
          case loaded_dynamic_nifs(app, info.ebin_dir) do
            [] ->
              []

            beams ->
              {:error, context} = err(:dynamic_nifs_loading, {app, beams})
              [context]
          end
        end)
        |> Enum.flat_map(fn {:ok, diagnostics} -> diagnostics end)

      manifest_path = Path.join(out_dir, "manifest.json")
      manifest_apps = Map.merge(packed_apps, manifest.apps)

      packed_tar_paths =
        packed_apps
        |> Map.values()
        |> Enum.map(&Path.expand(Path.join(out_dir, &1.tar)))

      tar_paths = maybe_strip_tarballs(packed_tar_paths ++ input_tar_paths, strip, out_dir)

      manifest = %{
        entrypoint: entrypoint_app,
        apps: manifest_apps,
        notes: diagnostics,
        toolchain: toolchain,
        vm: %{boot: "bin/vm.boot", version: vm_version}
      }

      File.write!(manifest_path, encode_json(manifest))

      result = %{
        ok: true,
        entrypoint: entrypoint_app,
        manifestPath: Path.expand(manifest_path),
        tarPaths: tar_paths,
        apps: manifest_apps,
        notes: diagnostics,
        toolchain: toolchain
      }

      {:ok, result}
    end
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

      {to_string(name), %{props: props, ebin_dir: dir}}
    end

    root_dir
    |> Path.join("*/ebin/*.app")
    |> Path.wildcard()
    |> Map.new(extract_info)
  end

  defp get_builtin_apps(toolchain) do
    elixir_apps = get_apps_info(toolchain.elixir_root)
    otp_apps = get_apps_info(toolchain.otp_root)

    Map.merge(elixir_apps, otp_apps)
  end

  defp read_manifest(manifest_path) do
    with {:ok, json} <- File.read(manifest_path),
         {:ok, %{"apps" => apps, "vm" => %{"version" => version}}} <- decode_json(json) do
      {:ok, %{version: version, apps: apps}}
    else
      _ -> err(:bad_manifest, manifest_path)
    end
  end

  defp apps_to_pack(project_apps, _builtin_apps, nil) do
    {:ok, Enum.sort_by(project_apps, fn {app, _info} -> app end)}
  end

  defp apps_to_pack(project_apps, builtin_apps, entrypoint)
       when is_map_key(project_apps, entrypoint) do
    all_apps_info = Map.merge(builtin_apps, project_apps)

    with {:ok, selected_apps} <-
           gather_required_apps(all_apps_info, project_apps, entrypoint, MapSet.new()) do
      project_apps
      |> Map.filter(fn {app, _info} -> MapSet.member?(selected_apps, app) end)
      |> Enum.sort()
      |> then(&{:ok, &1})
    end
  end

  defp apps_to_pack(_project_apps, _builtin_apps, entrypoint) do
    err(:missing_entrypoint, entrypoint)
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

  defp fetch_toolchain_info(runtime_version) do
    # host: computer this runs on
    # runtime: vm compiled to wasm
    host_version = host_otp_version()
    host = otp_version(host_version)
    runtime = otp_version(runtime_version)

    [host_major | _] = host
    [runtime_major | _] = runtime

    compatible = runtime_major - 2 <= host_major and version_lte?(host, runtime)

    if compatible do
      otp_root = Path.join(to_string(:code.root_dir()), "lib")
      elixir_root = :elixir |> :code.lib_dir() |> to_string() |> Path.dirname()

      info = %{
        otp: host_version,
        elixir: System.version(),
        otp_root: otp_root,
        elixir_root: elixir_root
      }

      {:ok, info}
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

  defp err(:bad_manifest, path) do
    {:error, %{code: "bad_manifest", path: path}}
  end

  defp err(:unsupported_otp, {host, runtime}) do
    {:error, %{code: "unsupported_otp", host: host, runtime: runtime}}
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
