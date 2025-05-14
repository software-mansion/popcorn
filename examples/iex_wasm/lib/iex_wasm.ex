defmodule IexWasm do
  use GenServer
  import Popcorn.Wasm
  alias Popcorn.Wasm

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl GenServer
  def init(_args) do
    Shell.start_link(type: :elixir)
#    Shell.start_link(type: :erlang)
    {:ok, nil}
  end
  
  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    new_state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, new_state}
  end
  
  defp handle_wasm({:wasm_call, [language, code]}, state) when language in ["erlang", "elixir"] do
    type = String.to_atom(language)
    try do
      :ok = ExTTY.send_text(:"#{type}_tty", code)
      {:resolve, :ok, state}
    rescue
      error -> {:resolve, error, state}
    end
  end
end
