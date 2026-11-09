defmodule LivebookWasm.Memsup do
  @moduledoc """
  Answers os_mon's memsup calls, which Livebook makes to show memory usage.

  memsup reads the OS's memory with OS programs, which the browser doesn't have.
  This reports the VM's memory against the WebAssembly memory limit.
  """
  use GenServer

  # wasm32's 4 GiB address space
  @total 2 ** 32

  def start_link(_args), do: GenServer.start_link(__MODULE__, nil, name: :memsup)

  @impl true
  def init(nil), do: {:ok, nil}

  @impl true
  def handle_call(:get_system_memory_data, _from, state) do
    free = @total - :erlang.memory(:total)
    data = [total_memory: @total, free_memory: free, system_total_memory: @total]
    {:reply, data, state}
  end
end
