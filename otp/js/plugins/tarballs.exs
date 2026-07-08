redirect_to_stderr = fn f ->
  stdout = Process.group_leader()
  Process.group_leader(self(), Process.whereis(:standard_error))
  f.()
  Process.group_leader(self(), stdout)
end

redirect_to_stderr.(fn ->
  Mix.install([{:jason, "~> 1.4"}])
end)

defmodule Tarballs do
  @options [
    root_dir: :string,
    entrypoint_app: :string,
    out_dir: :string,
    provided_apps_manifest_path: :string
  ]

  @required_options [:root_dir, :out_dir, :provided_apps_manifest_path]
  @static_nif_apps MapSet.new(["wasm"])

  def main(argv) do
    case run(argv) do
      {:ok, report} ->
        report
        |> Jason.encode!()
        |> IO.puts()

      {:error, error} ->
        %{ok: false, error: error}
        |> Jason.encode!()
        |> IO.puts()

        System.halt(1)
    end
  end

  defp run(argv) do
    with {:ok, args} <- parse_argv(argv),
         {:ok, provided_apps} <- fetch_provided_apps(args.provided_apps_manifest_path),
         {:ok, user_apps} <- fetch_user_apps(args.root_dir),
         {:ok, apps} <- fetch_apps_to_pack(user_apps, provided_apps.names, args.entrypoint_app) do
      vm_version = provided_apps.version

      File.mkdir_p!(args.out_dir)

      packed_apps =
        apps
        |> Task.async_stream(fn app ->
          info = Map.fetch!(user_apps, app)
          version = Keyword.get(info.props, :vsn, ~c"") |> to_string()
          tar_path = create_tarball(args.out_dir, app, info.ebin_dir)

          {app, %{tar: tar_path, version: version}}
        end)
        |> Map.new(fn {:ok, app} -> app end)

      dynamic_nifs =
        apps
        |> Task.async_stream(fn app ->
          info = Map.fetch!(user_apps, app)
          dynamic_nifs(app, info.ebin_dir)
        end)
        |> Enum.flat_map(fn {:ok, notes} -> notes end)

      notes = dynamic_nifs ++ otp_version(vm_version)
      manifest_path = Path.join(args.out_dir, "manifest.json")
      manifest_apps = Map.merge(packed_apps, provided_apps.apps)

      manifest = %{
        entrypoint: args.entrypoint_app,
        apps: manifest_apps,
        notes: notes,
        vm: %{boot: "bin/vm.boot", version: vm_version}
      }

      File.write!(manifest_path, Jason.encode!(manifest))

      {:ok,
       %{
         ok: true,
         entrypoint: args.entrypoint_app,
         manifestPath: Path.expand(manifest_path),
         apps: manifest_apps,
         notes: notes
       }}
    end
  end

  defp fetch_user_apps(root_dir) do
    build_lib_dir = Path.join([root_dir, "_build", "dev", "lib"])
    app_matcher = Path.join(build_lib_dir, "*/ebin/*.app")
    all_app_paths = Path.wildcard(app_matcher)

    {:ok,
     Map.new(all_app_paths, fn app_path ->
       {:ok, [{:application, name, props}]} = :file.consult(app_path)

       {to_string(name), %{props: props, ebin_dir: Path.dirname(app_path)}}
     end)}
  end

  defp fetch_provided_apps(manifest_path) do
    with {:ok, json} <- File.read(manifest_path),
         {:ok, %{"version" => version} = data} <- Jason.decode(json),
         {:ok, apps} <- normalize_provided_apps(data) do
      {:ok, %{version: version, apps: apps, names: apps |> Map.keys() |> MapSet.new()}}
    else
      _ -> err(:bad_provided_app_manifest, manifest_path)
    end
  end

  defp normalize_provided_apps(data) do
    data
    |> Map.delete("version")
    |> Enum.reduce_while({:ok, %{}}, fn
      {app, %{"tar" => tar, "version" => version} = entry}, {:ok, apps}
      when is_binary(tar) and is_binary(version) ->
        entry = Map.put(entry, "tar", normalize_tar_path(tar))
        {:cont, {:ok, Map.put(apps, app, entry)}}

      _entry, _acc ->
        {:halt, :error}
    end)
  end

  defp normalize_tar_path("/lib/" <> path), do: "lib/" <> path
  defp normalize_tar_path(tar_path), do: tar_path

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
              {:halt, err(:missing_dep, {app, dep})}
            end
        end)
    end
  end

  defp create_tarball(outdir, app, ebin_dir) do
    tar = "lib/#{app}.tar.gz"
    tar_path = Path.join(outdir, tar)
    tar_path_c = to_charlist(tar_path)
    arc_name = ~c"lib/#{app}/ebin"
    ebin_dir_c = to_charlist(ebin_dir)

    File.mkdir_p!(Path.dirname(tar_path))
    :ok = :erl_tar.create(tar_path_c, [{arc_name, ebin_dir_c}], [:compressed])

    tar
  end

  defp dynamic_nifs(app, ebin_dir) do
    if MapSet.member?(@static_nif_apps, app) do
      []
    else
      beams =
        Path.join(ebin_dir, "*.beam")
        |> Path.wildcard()
        |> Enum.filter(&imports_load_nif?/1)
        |> Enum.map(&Path.basename/1)

      case beams do
        [] -> []
        _ -> [%{code: "dynamic_nif", app: app, beams: beams}]
      end
    end
  end

  defp imports_load_nif?(beam_path) do
    case :beam_lib.chunks(to_charlist(beam_path), [:imports]) do
      {:ok, {_mod, [imports: imports]}} -> {:erlang, :load_nif, 2} in imports
      _ -> false
    end
  end

  defp otp_version(vm_version) do
    if otp_major(System.otp_release()) != otp_major(vm_version) do
      [%{code: "otp_mismatch", local: System.otp_release(), vm: vm_version}]
    else
      []
    end
  end

  defp otp_major(version) do
    version |> to_string() |> String.split(".") |> hd() |> String.to_integer()
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

  defp parse_argv(argv) do
    {opts, _rest, invalid} = OptionParser.parse(argv, strict: @options)

    missing_opts = Enum.reject(@required_options, &Keyword.has_key?(opts, &1))

    case {invalid, missing_opts} do
      {[], []} ->
        {:ok, opts |> Map.new() |> Map.put_new(:entrypoint_app, nil)}

      _ ->
        err(:bad_args, {invalid, missing_opts})
    end
  end

  defp err(:bad_args, {invalid, missing_opts}) do
    invalid =
      Enum.map(invalid, fn {option, value} ->
        %{option: option, value: value}
      end)

    missing = Enum.map(missing_opts, &to_string/1)

    {:error, %{code: "bad_args", invalid: invalid, missing: missing}}
  end

  defp err(:missing_entrypoint, app) do
    {:error, %{code: "missing_entrypoint", app: app}}
  end

  defp err(:bad_provided_app_manifest, path) do
    {:error, %{code: "bad_provided_app_manifest", path: path}}
  end

  defp err(:missing_dep, {app, dep}) do
    {:error, %{code: "missing_dep", app: app, dep: dep}}
  end
end

Tarballs.main(System.argv())
