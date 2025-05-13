defmodule EvalInWasm.Application do
  alias Popcorn.Wasm

  def start do
    {:ok, pid} = GenServer.start_link(EvalInWasm, [])
    Process.register(pid, :main)
    Wasm.register("main")
    IO.puts("Starting interpreter...")
    Process.sleep(:infinity)
  end
end
