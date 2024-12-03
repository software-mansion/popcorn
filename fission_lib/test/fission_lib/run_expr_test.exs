defmodule RunExprTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  test "run simple expression", %{tmp_dir: tmp_dir} do
    result =
      quote do
        var!(n) + 3
      end
      |> RunInAtomVM.expr(tmp_dir, n: 2)

    assert result == 5
  end
end
