defmodule IexWasm.Application do
  alias Popcorn.Wasm
  import ExTTY

  def start do
    {:ok, _pid} = :application_controller.start(:kernel)
    :kernel.start([],[])
    :application.ensure_all_started(:kernel)
    :application.ensure_all_started(:elixir)
    :application.ensure_all_started(:iex)
    {:ok, pid} = IexWasm.start_link([])
    Process.register(pid, :main)
    Wasm.register("main")
    Process.sleep(:infinity)
  end
  
end
