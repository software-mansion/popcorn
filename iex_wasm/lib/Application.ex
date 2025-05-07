defmodule App.Application do
  alias FissionLib.Wasm

  def start do
    {:ok, pid} = GenServer.start_link(App, [])
    Process.register(pid, :main)
    Wasm.register("main")
    IO.puts("Starting interpreter...")
    Process.sleep(:infinity)
  end
end
