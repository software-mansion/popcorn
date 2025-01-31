defmodule FissionLib.EvalTest do
  use ExUnit.Case, async: true
  require Logger
  require FissionLib.AtomVM
  import FissionLib.AsyncTest
  alias FissionLib.AtomVM

  async_test "add" do
    "1 + 2."
    |> AtomVM.eval(:erlang_expr)
    |> AtomVM.assert_result(3)
  end

  async_test "case" do
    """
    case lists:max([1,3,2]) of
      3 -> {max, 3};
      2 -> error
    end.
    """
    |> AtomVM.eval(:erlang_expr)
    |> AtomVM.assert_result({:max, 3})
  end

  async_test "send receive" do
    """
    self() ! message,

    receive
      message -> received
    end.
    """
    |> AtomVM.eval(:erlang_expr)
    |> AtomVM.assert_result(:received)
  end

  async_test "spawn" do
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
    |> AtomVM.eval(:erlang_expr)
    |> AtomVM.assert_result(:ok)
  end

  async_test "closure" do
    """
    A = fun() -> 5 end,
    B = 10,
    A() + B.
    """
    |> AtomVM.eval(:erlang_expr)
    |> AtomVM.assert_result(15)
  end
end
