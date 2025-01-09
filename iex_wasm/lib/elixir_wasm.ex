defmodule ElixirWasm do
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
end
