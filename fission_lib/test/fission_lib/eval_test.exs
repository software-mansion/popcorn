defmodule FissionLib.EvalTest do
  use ExUnit.Case, async: true
  require Logger

  @moduletag :tmp_dir
  setup_all do
    {_output, 0} = System.shell("elixirc -o tmp test/fixtures/eval.ex", stderr_to_stdout: true)

    FissionLib.pack(
      artifacts: ["tmp/Elixir.Eval.beam"],
      start_module: Eval,
      output_path: "tmp/eval.avm"
    )

    :ok
  end

  defp run(code, tmp_dir) do
    File.write!("tmp/code.erl", code)

    {_output, ret_val} =
      System.shell(
        "/Users/matheksm/ew/FissionVM/build/src/AtomVM tmp/eval.avm &> #{tmp_dir}/log.txt"
      )

    assert ret_val == 0,
           "Executing code with erl_eval failed, see #{Path.relative_to_cwd(tmp_dir)}/log.txt for logs"

    assert {:ok, result} = File.read!("tmp/result.bin") |> :erlang.binary_to_term()
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
