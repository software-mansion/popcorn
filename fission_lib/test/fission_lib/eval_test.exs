defmodule FissionLib.EvalTest do
  use ExUnit.Case, async: true
  require Logger
  require FissionLib.AtomVM
  alias FissionLib.AtomVM

  test "add" do
    "1 + 2."
    |> AtomVM.eval(:erlang_expr)
    |> AtomVM.assert_result(3)
  end

  test "case" do
    """
    case lists:max([1,3,2]) of
      3 -> {max, 3};
      2 -> error
    end.
    """
    |> AtomVM.eval(:erlang_expr)
    |> AtomVM.assert_result({:max, 3})
  end

  test "send receive" do
    """
    self() ! message,

    receive
      message -> received
    end.
    """
    |> AtomVM.eval(:erlang_expr)
    |> AtomVM.assert_result(:received)
  end

  test "spawn" do
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
end
