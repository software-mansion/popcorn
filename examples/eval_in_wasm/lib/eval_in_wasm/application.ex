defmodule EvalInWasm.Application do
  alias Popcorn.Wasm

  def start do
    :erlang.display("SIEAMA")
#    {:ok, pid} = GenServer.start_link(EvalInWasm, [])
#    Process.register(pid, :main)
#    Wasm.register("main")
#    IO.puts("Starting interpreter...")
#    Process.sleep(:infinity)
    :elixir.start([], [])
    try do
#      :erlang.raise(:error, {:case_clause,4}, [{:erlang,:apply,3,[{:file,'patches/estdlib/erlang.erl'},{:line,334}]}])
#      :erlang.error(:stacktrace)
      """
      defmodule Math do
       def double_each([head | tail]) do
         [head * 2 | double_each(tail)]
       end

       def double_each([]) do
         []
       end
      end

      Math.double_each([1, 2, 3])
      """
      |> Code.eval_string()
    rescue
      error -> {"SIEMA", error}
    end
  end
end
