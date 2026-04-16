defmodule Mix.Tasks.Llv.Build do
  use Mix.Task

  @shortdoc "Builds the local WASM bundle"

  @moduledoc """
  Builds the LocalLiveView WASM bundle from the `local/` project.

  ## Usage

      mix llv.build

  Equivalent to running `MIX_TARGET=wasm mix build` inside `local/`.

  You can also add this task to your `setup` alias in `mix.exs`:

      defp aliases do
        [
          setup: ["deps.get", "llv.build", ...]
        ]
      end
  """

  @impl Mix.Task
  def run(_args) do
    unless File.exists?("local") do
      Mix.raise("local/ directory not found. Run `mix llv.install` first.")
    end

    Mix.shell().cmd("MIX_TARGET=wasm mix build", cd: "local")
  end
end
