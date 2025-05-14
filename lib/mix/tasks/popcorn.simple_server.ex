defmodule Mix.Tasks.Popcorn.SimpleServer do
  use Mix.Task

  @priv_dir :code.priv_dir(:popcorn)

  @impl true
  def run(_args) do
    File.cp!(Path.join(@priv_dir, "server.exs"), "server.exs")
  end
end
