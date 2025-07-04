defmodule Popcorn.Utils.FetchArtifacts do
  @moduledoc false

  alias Popcorn.Utils.Downloader

  @artifatcs %{
    wasm: ["AtomVM.wasm", "AtomVM.mjs"],
    unix: ["AtomVM"]
  }

  @targets Map.keys(@artifatcs)

  @download_dir Path.join(Mix.Project.app_path(), "atomvm_artifacts")

  @spec download_artifacts([{:force, boolean}]) :: :ok
  def download_artifacts(opts \\ []) do
    for %{type: :url} = config <- parse_config() do
      out_dir = Path.join(@download_dir, "#{config.target}")
      cache_file = "#{out_dir}.cache"
      new_cache = config.location

      if opts[:force] or File.read(cache_file) != {:ok, new_cache} do
        File.rm(cache_file)
        artifacts = Map.fetch!(@artifatcs, config.target)
        File.rm_rf!(out_dir)
        File.mkdir_p!(out_dir)

        try do
          Downloader.start_inets_profile()
          Enum.each(artifacts, fn name -> download_artifact(config.location, out_dir, name) end)
        after
          Downloader.stop_inets_profile()
        end

        File.write!(cache_file, new_cache)
      end
    end
  end

  @spec fetch_artifacts(:unix | :wasm) :: [path :: String.t()]
  def fetch_artifacts(target) do
    config = Enum.find(parse_config(), &(&1.target == target))

    src_dir =
      case config.type do
        :path -> config.location
        :url -> Path.join(@download_dir, "#{config.target}")
      end

    for name <- Map.fetch!(@artifatcs, target) do
      Path.join(src_dir, name)
    end
  end

  @spec parse_config() :: [%{target: :unix | :wasm, location: String.t(), type: :path | :url}]
  defp parse_config() do
    config =
      Popcorn.Config.get(:runtime)
      |> List.wrap()
      |> Enum.map(fn
        {type, location} when type in [:path, :url] and is_binary(location) ->
          %{type: type, location: location, target: @targets}

        {type, location, opts}
        when type in [:path, :url] and is_binary(location) and is_list(opts) ->
          %{type: type, location: location, target: List.wrap(opts[:target] || @targets)}
      end)

    Enum.map(@targets, fn t -> %{Enum.find(config, &(t in &1.target)) | target: t} end)
  end

  defp download_artifact(url, dir, name) do
    path = Path.join(dir, name)
    tmp_path = path <> ".download"

    with {:ok, _stream} <-
           Downloader.download("#{url}/#{name}", File.stream!(tmp_path)) do
      File.rename!(tmp_path, path)
    else
      {:error, reason} ->
        raise """
        Couldn't download #{name} from #{url}, reason: #{reason}, \
        please use mix popcorn.build_runtime to build from source.
        """
    end
  end
end
