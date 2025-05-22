defmodule Shell do
  use GenServer
  import Popcorn.Wasm
  alias Popcorn.Wasm

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl GenServer
  def init(opts) do
    type = Keyword.get(opts, :type, :elixir)
    ExTTY.start_link(handler: self(), shell_opts: [dot_iex_path: ""], name: :"#{type}_tty", type: type)
    {:ok, %{type: type}}
  end

  @impl GenServer
  def handle_info({:tty_data, code_output}, %{type: type} = state) do
    """
    ({ window, args }) => {
      console.log(args.code_output);
      let ansiRegex = /[\\u001b\\u009b][[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]/g;
      let code_bunch = args.code_output.replace(ansiRegex, '');
      window.#{to_string(type)}_terminal.write(args.code_output)
    }
    """
    |> Wasm.run_js(args: %{code_output: code_output})
    {:noreply, state}
  end

end
