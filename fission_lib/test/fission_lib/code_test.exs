defmodule FissionLib.CodeTest do
  use ExUnit.Case, async: true
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @moduletag :tmp_dir

  async_test "code load module", %{tmp_dir: run_dir} do
    [{CodeTest.Foo, beam}] =
      quote do
        defmodule CodeTest.Foo do
          def foo(x), do: x + 1
        end
      end
      |> Code.compile_quoted()

    quote do
      :code.load_binary(CodeTest.Foo, ~c"", args.beam)
      apply(CodeTest.Foo, :foo, [2])
    end
    |> AtomVM.compile_quoted()
    |> AtomVM.run(run_dir, beam: beam)
    |> AtomVM.assert_result(3)
  end
end
