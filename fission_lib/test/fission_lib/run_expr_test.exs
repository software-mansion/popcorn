defmodule RunExprTest do
  use ExUnit.Case, async: true
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @moduletag :tmp_dir

  async_test "run simple expression", %{tmp_dir: tmp_dir} do
    quote do
      args.n + 3
    end
    |> AtomVM.compile_quoted()
    |> AtomVM.run(tmp_dir, n: 2)
    |> AtomVM.assert_result(5)
  end
end
