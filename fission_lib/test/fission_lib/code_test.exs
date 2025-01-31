defmodule FissionLib.CodeTest do
  use ExUnit.Case, async: true
  require FissionLib.AtomVM
  alias FissionLib.AtomVM

  test "code load module" do
    [{CodeTest.Foo, beam}] =
      quote do
        defmodule CodeTest.Foo do
          def foo(x), do: x + 1
        end
      end
      |> Code.compile_quoted()

    info =
      quote do
        :code.load_binary(CodeTest.Foo, ~c"", var!(beam))
        apply(CodeTest.Foo, :foo, [2])
      end
      |> AtomVM.compile_quoted([:beam])
      |> AtomVM.run_with_bindings(beam: beam)

    assert info.result == 3
  end
end
