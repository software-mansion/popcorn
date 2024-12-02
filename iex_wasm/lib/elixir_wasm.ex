defmodule ElixirWasm do
  def start() do
    :elixir.start([], [])
    spawn(&loop/0)
  end

  defp loop() do
    receive do
      {:emscripten, {:call, promise, data}} ->
        result =
          case Code.eval_string(data, [], __ENV__) do
            {result, _binding} -> result
            other -> other
          end

        result_str = :io_lib.format(~c"~p", [result])
        :emscripten.promise_resolve(promise, result_str)
    end

    loop()
  end
end
