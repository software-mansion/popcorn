defmodule Mix.Tasks.Compile.Popcorn do
  use Mix.Task.Compiler

  @impl true
  def run(_args) do
    Popcorn.pack()
  end
end
