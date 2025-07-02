defmodule Popcorn.CompilationTest do
  use ExUnit.Case, async: true
  require Popcorn.Support.AtomVM
  alias Popcorn.Support.AtomVM

  @moduletag :tmp_dir

  test "code load module", %{tmp_dir: run_dir} do
    module_ast =
      quote do
        defmodule CodeTest.Foo do
          def foo(x), do: x + 1
        end
      end

    [{CodeTest.Foo, beam}] = Code.compile_quoted(module_ast)

    quote do
      :code.load_binary(CodeTest.Foo, ~c"nofile", args.beam)
      apply(CodeTest.Foo, :foo, [2])
    end
    |> AtomVM.compile_quoted()
    |> AtomVM.run(run_dir, beam: beam)
    |> AtomVM.assert_result(3)
  end

  test "run simple expression", %{tmp_dir: tmp_dir} do
    quote do
      args.n + 3
    end
    |> AtomVM.compile_quoted()
    |> AtomVM.run(tmp_dir, n: 2)
    |> AtomVM.assert_result(5)
  end

  test "stacktrace", %{tmp_dir: run_dir} do
    info =
      quote do
        try do
          :orddict.take(:not_an_orddict, 1)
        rescue
          e -> {e, __STACKTRACE__}
        end
      end
      |> AtomVM.compile_quoted()
      |> AtomVM.try_run(run_dir)

    {error, stacktrace} = info.result

    assert %FunctionClauseError{module: :orddict, function: :take, arity: 2} = error

    assert [
             {:orddict, :take, 2, [file: ~c"orddict", line: 118]},
             {RunExpr, :run, 1, [file: code_file, line: run_line]}
             | rest
           ] = stacktrace

    assert {RunExpr, :start, 0, [file: ^code_file, line: start_line]} = List.last(rest)

    lines = [1, start_line, run_line, 1000]
    assert lines == Enum.sort(lines)
    assert code_file |> to_string() |> String.ends_with?("/code.ex")
  end
end
