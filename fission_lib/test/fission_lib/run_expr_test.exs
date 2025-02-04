defmodule RunExprTest do
  use ExUnit.Case, async: true
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

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
