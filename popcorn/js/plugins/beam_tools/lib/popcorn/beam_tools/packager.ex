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

    with {:ok, manifest} <- fetch_manifest(manifest_path),
         {:ok, toolchain} <- compatible_toolchain(manifest.version),
         {:ok, user_apps} <- fetch_user_apps(root_dir),
         manifest_names = manifest.apps |> Map.keys() |> MapSet.new(),
         {:ok, apps} <- fetch_apps_to_pack(user_apps, manifest_names, entrypoint_app) do
      vm_version = manifest.version

      File.mkdir_p!(out_dir)

      packed_apps =
        apps
        |> Task.async_stream(fn app ->
          info = Map.fetch!(user_apps, app)
          version = Keyword.get(info.props, :vsn, ~c"") |> to_string()
          tar_path = create_tarball(out_dir, app, info.ebin_dir)

          {app, %{tar: tar_path, version: version}}
        end)
        |> Map.new(fn {:ok, app} -> app end)

      diagnostics =
        apps_info.project
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

      {:ok,
       %{
         ok: true,
         entrypoint: entrypoint_app,
         manifestPath: Path.expand(manifest_path),
         tarPaths: tar_paths,
         apps: manifest_apps,
         notes: diagnostics,
         toolchain: toolchain
       }}
    end
  end

  defp maybe_strip_tarballs(paths, false, _out_dir), do: paths

  defp maybe_strip_tarballs(paths, true, out_dir) do
    Enum.map(paths, fn path ->
      :ok = strip_tarball(path, out_dir)
      Path.expand(Path.join(out_dir, Path.basename(path)))
    end)
  end

  defp fetch_user_apps(root_dir) do
    build_env = System.get_env("MIX_ENV", "dev")
    build_lib_dir = Path.join([root_dir, "_build", build_env, "lib"])
    app_matcher = Path.join(build_lib_dir, "*/ebin/*.app")
    all_app_paths = Path.wildcard(app_matcher)

    {:ok,
     Map.new(all_app_paths, fn app_path ->
       {:ok, [{:application, name, props}]} = :file.consult(app_path)

       {to_string(name), %{props: props, ebin_dir: Path.dirname(app_path)}}
     end)}
  end

  defp fetch_manifest(manifest_path) do
    with {:ok, json} <- File.read(manifest_path),
         {:ok, %{"apps" => apps, "vm" => %{"version" => version}}} <- decode_json(json) do
      {:ok, %{version: version, apps: apps}}
    else
      _ -> err(:bad_manifest, manifest_path)
    end
  end

  defp fetch_apps_to_pack(user_apps, _provided_apps, nil) do
    {:ok, user_apps |> Map.keys() |> Enum.sort()}
  end

  defp fetch_apps_to_pack(user_apps, provided_apps, entrypoint_app) do
    if Map.has_key?(user_apps, entrypoint_app) do
      case fetch_needed_apps(user_apps, provided_apps, entrypoint_app, MapSet.new()) do
        {:ok, apps} -> {:ok, apps |> MapSet.to_list() |> Enum.sort()}
        {:error, _} = error -> error
      end
    else
      err(:missing_entrypoint, entrypoint_app)
    end
  end

  defp fetch_needed_apps(user_apps, provided_apps, app, required) do
    cond do
      MapSet.member?(required, app) ->
        {:ok, required}

      MapSet.member?(provided_apps, app) ->
        {:ok, required}

      true ->
        info = Map.fetch!(user_apps, app)
        required = MapSet.put(required, app)

        info.props
        |> get_required_apps()
        |> Enum.reduce_while({:ok, required}, fn
          dep, {:ok, acc} ->
            available = MapSet.member?(provided_apps, dep) or Map.has_key?(user_apps, dep)

            if available do
              case fetch_needed_apps(user_apps, provided_apps, dep, acc) do
                {:ok, acc} -> {:cont, {:ok, acc}}
                {:error, _} = error -> {:halt, error}
              end
            else
              {:halt, err(:missing_dep, {app, dep, provided_apps, user_apps})}
            end
        end)
    end
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

  defp compatible_toolchain(runtime_version) do
    host_version = host_otp_version()
    host = otp_version(host_version)
    runtime = otp_version(runtime_version)

    if hd(host) >= hd(runtime) - 2 and version_lte?(host, runtime) do
      {:ok, %{otp: host_version, elixir: System.version()}}
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

  defp err(:missing_dep, {app, dep, provided_apps, user_apps}) do
    {:error,
     %{
       code: "missing_dep",
       app: app,
       dep: dep,
       provided_apps: provided_apps |> MapSet.to_list() |> Enum.sort(),
       user_apps: user_apps |> Map.keys() |> Enum.sort()
     }}
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
