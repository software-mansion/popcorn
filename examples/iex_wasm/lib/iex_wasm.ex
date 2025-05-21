defmodule IexWasm do
  use GenServer
  import Popcorn.Wasm
  alias Popcorn.Wasm
  
  defguard is_language(language) when language in ["erlang", "elixir"]

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl GenServer
  def init(_args) do
    {:ok, nil}
  end
  
  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    new_state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, new_state}
  end

  defp handle_wasm({:wasm_call, %{"command" => "code_data", "language" => language} = req}, state) when is_language(language) do
    type = String.to_atom(language)
    try do
      :ok = ExTTY.send_text(:"#{type}_tty", req["code"])
      {:resolve, :ok, state}
    rescue
      error -> {:resolve, error, state}
    end
  end
  
  defp handle_wasm({:wasm_call, %{"command" => "start", "language" => language}}, state) when is_language(language) do
    type = String.to_atom(language)
    Shell.start_link(type: type)
    {:resolve, :ok, state}
  end
  
end
