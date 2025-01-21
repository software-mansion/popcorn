defmodule ElixirModules do
  @factorial_module """
  defmodule Factorial do
    def calc(0), do: 1
    def calc(n) when n > 0, do: n * calc(n - 1)
  end
  """

  @fibonacci_module """
  defmodule Fibonacci do
      def calc(0) do 0 end
      def calc(1) do 1 end
      def calc(n) do fib(n-1) + fib(n-2) end
  end
  """

  @simple_module """
  defmodule Adder do
    def calc(_a, _b), do: :added
  end
  """

  def start() do
    :elixir.start([], [])

    (@simple_module <> "\Adder.calc(10, 20)\n")
    |> eval()
    |> print()
  end

  defp eval(string) do
    string
    |> Code.eval_string([], __ENV__)
    |> elem(0)
  rescue
    error ->
      {:error, error}
  end

  defp print(term) do
    :erlang.display({~c"____________ElixirModules____________", term})
  end
end
