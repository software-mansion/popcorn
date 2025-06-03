defmodule Shell do
  use GenServer
  alias Popcorn.Wasm

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl GenServer
  def init(opts) do
    type = Keyword.get(opts, :type, :elixir)

    ExTTY.start_link(
      handler: self(),
      shell_opts: [dot_iex_path: ""],
      name: :"#{type}_tty",
      type: type
    )

    {:ok, %{type: type}}
  end

  @impl GenServer
  def handle_info({:tty_data, code_output}, state) do
    """
    ({ window, args }) => {
      window.terminal.write(args.code_output)
    }
    """
    |> Wasm.run_js(args: %{code_output: code_output})

    {:noreply, state}
  end
end
