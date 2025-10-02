defmodule Popcorn.ArtifactsCache do
  @moduledoc """
  Artifacts cache is a map, stored in external term format on disk. Stores input files with hashes for the previous build.
  Used to skip building parts of input files if they did not change.

  The stored map has following structure:
  %{
    app1: %{
      file_path1: hash1,
      file_path2: hash2
    },
    app2: %{
      ...
    }
  }
  """

  @app_path Mix.Project.app_path()
  @cache_path "#{@app_path}/popcorn_patch_cache"

  @doc """
  Checks if cache is a subset of files to build.

  This is used to quickly check if cache is viable to use – if any cached file was deleted, we treat cache as invalidated.
  """
  def subset_of_sources?(cache, source_paths) do
    all_cached_paths =
      cache
      |> Map.values()
      |> Enum.flat_map(&Map.keys/1)
      |> MapSet.new()

    MapSet.subset?(all_cached_paths, source_paths)
  end

  @doc """
  Returns a map of apps from new cache with files changed compared to current cache.
  If app's files did not change, the app is not included.

  ## Examples

  ```elixir
  iex> cache = %{
  ...>   app1: %{
  ...>     "file1.ex" => "hash1",
  ...>     "file2.ex" => "hash2"
  ...>   },
  ...>   app2: %{
  ...>     "file3.ex" => "hash3"
  ...>   }
  ...> }
  iex> new_cache = %{
  ...>   app1: %{
  ...>     "file1.ex" => "new_hash1",
  ...>     "file2.ex" => "hash2"
  ...>   },
  ...>   app2: %{
  ...>     "file3.ex" => "hash3"
  ...>   },
  ...>   app3: %{
  ...>     "file4.ex" => "hash4"
  ...>   }
  ...> }
  iex> get_modified_apps(cache, new_cache)
  %{app1: ["file1.ex"], app3: ["file4.ex"]}
  ```
  """
  def get_modified_apps(cache, new_cache) do
    Enum.map(new_cache, fn {app, new_file_hashes} ->
      modified_paths = get_modified_paths(cache[app], new_file_hashes)

      {app, modified_paths}
    end)
    |> Enum.filter(fn {_app, modified_paths} -> modified_paths != [] end)
    |> Map.new()
  end

  def get_modified_paths(nil, new_file_hashes), do: Map.keys(new_file_hashes)

  def get_modified_paths(file_hashes, new_file_hashes) do
    new_file_hashes
    |> Map.filter(fn {new_path, new_hash} ->
      hash_changed = file_hashes[new_path] != new_hash
      hash_changed
    end)
    |> Map.keys()
  end

  @doc """
  Reads the map from disk or returns empty map if not persisted.
  """
  def read_from_disk!() do
    case File.read(@cache_path) do
      {:ok, cache} ->
        :erlang.binary_to_term(cache)

      {:error, :enoent} ->
        %{}

      {:error, reason} ->
        raise(File.Error,
          reason: reason,
          path: @cache_path
        )
    end
  end

  @doc """
  Writes the map to disk in external term format.
  """
  def write_to_disk!(cache) do
    File.write!(@cache_path, :erlang.term_to_binary(cache))
    :ok
  end

  @doc """
  Removes persisted cache or does nothing if already doesn't exist.
  """
  def drop_cache!() do
    case File.rm(@cache_path) do
      :ok ->
        :ok

      {:error, :enoent} ->
        :ok

      {:error, reason} ->
        raise(File.Error,
          reason: reason,
          path: @cache_path
        )
    end
  end
end
