defmodule EvalElixir.Evaluator do
  use GenServer

  @type bindings :: Keyword.t()

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @spec eval(String.t(), bindings()) ::
          {:ok, result :: any(), bindings()} | {:error, message :: String.t()}
  def eval(code, bindings \\ []) do
    do_eval(code, bindings, true)
  end

  defp do_eval(code, bindings, retry) do
    try do
      GenServer.call(__MODULE__, {:eval, code, bindings}, :infinity)
    catch
      :exit, {:noproc, _info} when retry ->
        Process.sleep(100)
        do_eval(code, bindings, false)

      :exit, reason ->
        {:error, inspect(reason)}
    end
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
            module: nil,
            function: nil
        })

      {:reply, {:ok, result, new_bindings}, state}
    rescue
      error ->
        error_message = Exception.format(:error, error)
        {:reply, {:error, error_message}, state}
    end
  end
end
