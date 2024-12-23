defmodule Mix.Tasks.Compile.FissionLib do
  use Mix.Task.Compiler

  @impl true
  def run(_args) do
    FissionLib.pack()
  end
end
