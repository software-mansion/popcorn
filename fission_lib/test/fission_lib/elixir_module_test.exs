defmodule FissionLib.ElixirModuleTest do
  use ExUnit.Case, async: true
  require Logger
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @examples_path "./test/examples"
  @moduletag :tmp_dir

  async_test "Factorial", %{tmp_dir: dir} do
    """
    defmodule Factorial do
      def calc(0), do: 1
      def calc(n) when n > 0, do: n * calc(n - 1)
    end

    Factorial.calc(5)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(120)
  end

  async_test "Fibonacci", %{tmp_dir: dir} do
    """
    defmodule Fibonacci do
        def calc(0) do 0 end
        def calc(1) do 1 end
        def calc(n) do calc(n-1) + calc(n-2) end
    end

    Fibonacci.calc(10)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(55)
  end

  async_test "Adder", %{tmp_dir: dir} do
    """
    defmodule Adder do
      def calc(a, b), do: a + b
    end

    Adder.calc(10, 20)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(30)
  end

  async_test "Guard", %{tmp_dir: dir} do
    """
    defmodule Guard do
      def f(n) when n > 0, do: f(n-1)
      def f(0), do: :done
    end

    Guard.f(5)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(:done)
  end

  async_test "MultipleFns", %{tmp_dir: dir} do
    """
    defmodule MultipleFns do
      def a(x), do: MultipleFns.b(x) + 1
      def b(x), do: MultipleFns.c(x) + 1
      def c(x), do: MultipleFns.d(x) + 1
      def d(x), do: x
    end

    MultipleFns.a(0)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(3)
  end

  async_test "simple module", %{tmp_dir: dir} do
    """
    defmodule Start do

    end
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "Capybara habitat - genserver", %{tmp_dir: dir} do
    "#{@examples_path}/CapybaraHabitat.ex"
    |> File.read!()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  async_test "lists_duplicate", %{tmp_dir: dir} do
    """
    :lists.duplicate(0, "a")
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result([])

    """
    :lists.duplicate(1, "a")
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(["a"])

    """
    :lists.duplicate(7, "a")
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(["a", "a", "a", "a", "a", "a", "a"])
  end
end
