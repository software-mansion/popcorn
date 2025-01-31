defmodule FissionLib.EvalTest do
  use ExUnit.Case, async: true
  require Logger
  alias FissionLib.AtomVM

  @moduletag :tmp_dir
  setup_all do
    quote do
      code = var!(code) |> :erlang.binary_to_list()
      {:ok, tokens, _} = :erl_scan.string(code)
      IO.puts("Scanned")
      {:ok, exprs} = :erl_parse.parse_exprs(tokens)
      IO.puts("Parsed")

      case :erl_eval.exprs(exprs, []) do
        {:value, value, _new_bindings} -> {:ok, value}
        other -> {:error, other}
      end
    end
    |> AtomVM.compile("tmp", [:code])

    :ok
  end

  defp run(code, tmp_dir) do
    assert {:ok, result} = AtomVM.run("tmp", tmp_dir, code: code)
    result
  end

  defp assert_ok(x), do: assert(x == :ok)

  test "add", %{tmp_dir: tmp_dir} do
    assert 3 == run("1 + 2.", tmp_dir)
  end

  test "case", %{tmp_dir: tmp_dir} do
    """
    case lists:max([1,3,2]) of
      3 -> ok;
      2 -> error
    end.
    """
    |> run(tmp_dir)
    |> assert_ok()
  end

  test "send receive", %{tmp_dir: tmp_dir} do
    """
    self() ! message,

    receive
      message -> ok
    end.
    """
    |> run(tmp_dir)
    |> assert_ok()
  end

  test "spawn", %{tmp_dir: tmp_dir} do
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
    |> run(tmp_dir)
    |> assert_ok()
  end
end
