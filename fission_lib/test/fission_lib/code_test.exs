defmodule FissionLib.CodeTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  test "code load module", %{tmp_dir: tmp_dir} do
    [{CodeTest.Foo, beam}] =
      quote do
        defmodule CodeTest.Foo do
          def foo(x), do: x + 1
        end
      end
      |> Utils.compile_quoted(tmp_dir)

    result =
      quote do
        :code.load_binary(CodeTest.Foo, ~c"", var!(beam))
        apply(CodeTest.Foo, :foo, [2])
      end
      |> RunInAtomVM.expr(tmp_dir, beam: beam)

    assert result == 3
  end
end
