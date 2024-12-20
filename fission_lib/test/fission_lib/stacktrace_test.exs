defmodule FissionLib.StackTraceTest do
  use ExUnit.Case, async: true
  @moduletag :tmp_dir

  test "stacktrace", %{tmp_dir: tmp_dir} do
    {error, stacktrace} =
      quote do
        try do
          :orddict.take(:not_an_orddict, 1)
        rescue
          e -> {e, __STACKTRACE__}
        end
      end
      |> RunInAtomVM.expr(tmp_dir)

    assert %FunctionClauseError{module: :orddict, function: :take, arity: 2} = error

    assert [
             {:orddict, :take, 2, [file: ~c"orddict", line: 118]},
             {RunExpr, :run, 1, [file: code_file, line: run_line]},
             {RunExpr, :start, 0, [file: code_file, line: start_line]}
           ] = stacktrace

    lines = [1, start_line, run_line, 1000]
    assert lines == Enum.sort(lines)

    assert code_file |> to_string() |> String.ends_with?("/code.ex")
  end
end
