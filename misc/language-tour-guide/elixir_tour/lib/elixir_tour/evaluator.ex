defmodule ElixirTour.Evaluator do
  use GenServer

  @type bindings :: Keyword.t()

  @spec start!() :: pid()
  def start!() do
    {:ok, pid} = GenServer.start(__MODULE__, [])
    pid
  end

  @spec eval(pid(), String.t(), bindings()) ::
          {:ok, result :: any(), bindings()} | {:error, message :: String.t()}
  def eval(runner, code, bindings) do
    GenServer.call(runner, {:eval, code, bindings}, :infinity)
  end

  @impl true
  def init(_opts) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:eval, code, bindings}, _from, state) do
    try do
      {result, new_bindings} =
        Code.eval_string(code, bindings, %Macro.Env{
          __ENV__
          | file: "playground",
            line: 1,
            module: nil
        })

      {:reply, {:ok, result, new_bindings}, state}
    rescue
      error ->
        error_message = Exception.format(:error, error)
        {:reply, {:error, error_message}, state}
    end
  end
end
