defmodule ElixirWasm do
  # def start() do
  #   spawn(&loop/0)
  # end
  use Bitwise

  def start() do
    :elixir.start([], [])

    data = """
    defmodule Adder do
      def add(a,b) do
        nil
      end
    end
    """

    case Code.eval_string(data, [], __ENV__) do
      {result, _binding} -> result
      other -> other
    end
    |> :erlang.display()

    :erlang.display("finish")
  end

  defp loop() do
    receive do
      {:emscripten, {:call, promise, data}} ->
        if :elixir_config |> Process.whereis() |> is_nil(), do: :elixir.start([], [])

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
