defmodule FissionLib.CodeTest do
  use ExUnit.Case

  @moduletag :tmp_dir

  test "code load module", %{tmp_dir: tmp_dir} do
    [{Foo, beam}] =
      quote do
        defmodule Foo do
          def foo(x), do: x + 1
        end
      end
      |> Code.compile_quoted()

    result =
      quote do
        :code.load_binary(Foo, ~c"", var!(beam))
        Foo.foo(2)
      end
      |> RunInAtomVM.expr(tmp_dir, beam: beam)

    assert result == 3
  end
end
