defmodule FissionLib.EvalTest do
  use ExUnit.Case, async: true
  require Logger
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @examples_path "./test/examples"
  @moduletag :tmp_dir

  async_test "add", %{tmp_dir: dir} do
    "1 + 2."
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result(3)
  end

  async_test "case", %{tmp_dir: dir} do
    """
    case lists:max([1,3,2]) of
      3 -> {max, 3};
      2 -> error
    end.
    """
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result({:max, 3})
  end

  async_test "send receive", %{tmp_dir: dir} do
    """
    self() ! message,

    receive
      message -> received
    end.
    """
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result(:received)
  end

  async_test "spawn", %{tmp_dir: dir} do
    """
    Parent = self(),

    Child = spawn(fun() ->
      receive
        ping -> Parent ! pong
      end
    end),

    Child ! ping,

    receive
       pong -> ok
    end,
    ok.
    """
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result(:ok)
  end

  async_test "closure", %{tmp_dir: dir} do
    """
    A = fun() -> 5 end,
    B = 10,
    A() + B.
    """
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result(15)
  end

  async_test ":lists.duplicate/2", %{tmp_dir: dir} do
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

  @tag :skip
  async_test "io_lib", %{tmp_dir: dir} do
    """
    term = {:ok, ["a", 2, 3.0]}
    ~c"~p"
    |> :io_lib.format([term])
    |> to_string()
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result("{ok,[<<\"a\">>,2,3.0]}")
  end

  async_test "rescue", %{tmp_dir: dir} do
    """
    try do
      1 + :ok
    rescue
      e -> e
    end
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%ArithmeticError{message: "bad argument in arithmetic expression"})
  end

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

  async_test "warnings", %{tmp_dir: dir} do
    info =
      """
      defmodule W do
        def foo(a, b, c), do: {a, b}
      end

      res1 = W.foo(10, 20, 30)

      defmodule W do
        def bar(a, b, c), do: {b, c}
      end

      res2 = W.bar(10, 20, 30)
      {res1, res2}
      """
      |> AtomVM.try_eval(:elixir, run_dir: dir)

    assert %{exit_status: 0, result: {{10, 20}, {20, 30}}} = info
    logs = File.read!(info.log_path)
    assert String.contains?(logs, "warning: {unused_var,c,false}")
    assert String.contains?(logs, "warning: {module_defined,'Elixir.RunExpr.W'}")
    assert String.contains?(logs, "warning: {unused_var,a,false}")
  end

  async_test "Adder", %{tmp_dir: dir} do
    """
    -module(adder).
    -export([add/2]).

    add(A, B) ->
        A + B.
    """
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "line location", %{tmp_dir: dir} do
    """
    -module(long_function).
    -export([start/0]).

    start() ->
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose".
    """
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "UUID - too big literals", %{tmp_dir: dir} do
    "#{@examples_path}/uuid.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "Greetings - message passing", %{tmp_dir: dir} do
    "#{@examples_path}/greetings.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "Capybara habitat - message passing", %{tmp_dir: dir} do
    "#{@examples_path}/capybara_habitat.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end
end
