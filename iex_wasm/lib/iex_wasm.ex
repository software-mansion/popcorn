defmodule IexWasm do
  # this compiler option is to suppress warnings when compiling the VM
  # it is not needed or recommended for user apps.
  @compile {:no_warn_undefined, [Console]}

  def start() do
    Console.print("Hello World\n")
    Process.register(self(), :main)
    loop()
  end

  defp loop() do
    receive do
      {:emscripten, {:call, promise, data}} ->
        result =
          case eval(:erlang.binary_to_list(data)) do
            {:value, value, _} -> value
            other -> other
          end

        result_str = :io_lib.format(~c"~p", [result])
        :emscripten.promise_resolve(promise, result_str)
    end

    loop()
  end

  defp eval(string) do
    try do
      {:ok, tokens, _} = :erl_scan.string(string)
      {:ok, exprs} = :erl_parse.parse_exprs(tokens)
      :erl_eval.exprs(exprs, [])
    catch
      error -> {:error, error}
    end
  end
end
