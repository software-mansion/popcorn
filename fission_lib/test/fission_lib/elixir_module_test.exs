defmodule FissionLib.ElixirModuleTest do
  use ExUnit.Case, async: true
  require Logger
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @examples_path "./test/examples"

  async_test "Factorial" do
    """
    defmodule Factorial do
      def calc(0), do: 1
      def calc(n) when n > 0, do: n * calc(n - 1)
    end

    Factorial.calc(5)
    """
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_result(120)
  end

  async_test "Fibonacci" do
    """
    defmodule Fibonacci do
        def calc(0) do 0 end
        def calc(1) do 1 end
        def calc(n) do calc(n-1) + calc(n-2) end
    end

    Fibonacci.calc(10)
    """
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_result(55)
  end

  async_test "Adder" do
    """
    defmodule Adder do
      def calc(a, b), do: a + b
    end

    Adder.calc(10, 20)
    """
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_result(30)
  end

  async_test "Guard" do
    """
    defmodule Guard do
      def f(n) when n > 0, do: f(n-1)
      def f(0), do: :done
    end

    Guard.f(5)
    """
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_result(:done)
  end

  async_test "Guard" do
    """
    defmodule MultipleFns do
      def a(x), do: MultipleFns.b(x) + 1
      def b(x), do: MultipleFns.c(x) + 1
      def c(x), do: MultipleFns.d(x) + 1
      def d(x), do: x
    end

    MultipleFns.a(0)
    """
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_result(3)
  end

  async_test "simple module" do
    """
    defmodule Start do

    end
    """
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_is_module()
  end

  async_test "Capybara habitat - genserver" do
    "#{@examples_path}/CapybaraHabitat.ex"
    |> File.read!()
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_is_module()
  end
end
