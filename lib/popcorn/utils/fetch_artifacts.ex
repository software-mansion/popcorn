defmodule Popcorn.Utils.FetchArtifacts do
  @moduledoc false

  @artifatcs %{
    wasm: ["AtomVM.wasm", "AtomVM.mjs"],
    unix: ["AtomVM"]
  }

  Application.compile_env(:popcorn, :runtime)

  alias Popcorn.Utils.Downloader

  def fetch_artifacts() do
    runtime_specs =
      Popcorn.Config.get(:runtime)
      |> List.wrap()
      |> Enum.map(fn
        {type, location} when type in [:path, :url] and is_binary(location) ->
          {type, location, []}

        {type, location, opts}
        when type in [:path, :url] and is_binary(location) and is_list(opts) ->
          {type, location, opts}
      end)

    for {type, location, opts} <- runtime_specs,
        target <- opts |> Keyword.get(:target, [:unix, :wasm]) |> List.wrap() do
      dir = Path.join(Mix.Project.app_path(), "atomvm_artifacts/#{target}")
      File.mkdir_p!(dir)
      artifacts = Map.fetch!(@artifatcs, target)
      paths = Enum.map(artifacts, &Path.join(dir, &1))

      if not Enum.all?(paths, &File.exists?/1) do
        do_fetch_artifacts(type, location, dir, artifacts)
      end
    end
  end

  defp do_fetch_artifacts(:url, url, dir, artifacts) do
    try do
      Downloader.start_inets_profile()
      Enum.each(artifacts, fn name -> download_artifact(url, dir, name) end)
    after
      Downloader.stop_inets_profile()
    end
  end

  defp do_fetch_artifacts(:path, location, dir, artifacts) do
    Enum.each(artifacts, fn name ->
      path = Path.join(location, name)

      if File.exists?(path) do
        File.cp!(path, Path.join(dir, name))
      else
        IO.warn("""
        Couldn't find runtime file #{name} in #{location} \
        please use mix popcorn.build_runtime to build from source.
        """)
      end
    end)
  end

  defp download_artifact(url, dir, name) do
    path = Path.join(dir, name)
    tmp_path = path <> ".download"

    with {:ok, _stream} <-
           Downloader.download("#{url}/#{name}", File.stream!(tmp_path)) do
      File.rename!(tmp_path, path)
    else
      {:error, reason} ->
        IO.warn("""
        Couldn't download #{name} from #{url}, reason: #{reason}, \
        please use mix popcorn.build_runtime to build from source.
        """)
    end
  end
end
