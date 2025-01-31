defmodule RunExprTest do
  use ExUnit.Case, async: true
  require FissionLib.AtomVM
  import FissionLib.AsyncTest
  alias FissionLib.AtomVM

  async_test "run simple expression" do
    info =
      quote do
        var!(n) + 3
      end
      |> AtomVM.compile_quoted([:n])
      |> AtomVM.run_with_bindings(n: 2)

    assert 5 = info.result
  end
end
