defmodule IexWasm do
  use GenServer
  import Popcorn.Wasm
  alias Popcorn.Wasm

  defguard is_language(language) when language in ["erlang", "elixir"]

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl GenServer
  def init(_args) do
    Wasm.register(Atom.to_string(@process_name))
    type = "elixir"
    Shell.start_link(type: type)
    :ok = ExTTY.send_text(:"#{type}_tty", ":ok\n")
    {:ok, nil}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, state}
  end

  defp handle_wasm({:wasm_call, %{"command" => "start", "language" => language}}, state)
       when is_language(language) do
    type = String.to_atom(language)
    Shell.start_link(type: type)
    {:resolve, :ok, state}
  end

  defp handle_wasm({:wasm_call, %{"command" => "code_data", "language" => language} = req}, state)
       when is_language(language) do
    type = String.to_atom(language)

    try do
      :ok = ExTTY.send_text(:"#{type}_tty", req["text"])
      {:resolve, :ok, state}
    rescue
      error -> {:resolve, error, state}
    end
  end
end
