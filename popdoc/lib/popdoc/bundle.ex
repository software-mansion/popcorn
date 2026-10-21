defmodule Popdoc.Bundle do
  @moduledoc false

  @assets_dir Path.expand("../../assets", __DIR__)

  def build do
    Mix.Task.run("compile")
    out_dir = Path.join(Mix.Project.build_path(), "popdoc")

    case Popcorn.Packager.build(app: "popdoc", out_dir: out_dir) do
      {:ok, _report} -> :ok
      {:error, error} -> Mix.raise(Popcorn.Packager.format_error(error))
    end

    for asset <- ~w(popdoc.js popdoc.css) do
      File.cp!(Path.join(@assets_dir, asset), Path.join(out_dir, asset))
    end

    out_dir
  end
end
