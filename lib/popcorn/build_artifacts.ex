defmodule Popcorn.BuildArtifacts do
  @app_path Mix.Project.app_path()
  @patches_path "#{@app_path}/popcorn_patches"

  def bundle_path(app), do: Path.join([@patches_path, "#{app}.avm"])
  def beams_dir(app), do: Path.join([@patches_path, to_string(app), "ebin"])

  def beam_paths(app) do
    app
    |> beams_dir()
    |> Path.join("*.beam")
    |> Path.wildcard()
  end

  def create_build_dirs!(apps) do
    File.mkdir_p!(@patches_path)

    process_async(apps, fn app ->
      app |> beams_dir() |> File.mkdir_p!()
    end)
  end

  def delete!(apps) do
    process_async(apps, fn app ->
      # we don't care when bundle doesn't exist
      app |> bundle_path() |> File.rm()
      app |> beams_dir() |> File.rm_rf!()
    end)
  end

  defp process_async(enum, fun, opts \\ []) do
    enum
    |> Task.async_stream(fun, Keyword.merge([timeout: 120_000, ordered: false], opts))
    |> Enum.map(fn {:ok, result} -> result end)
  end
end
