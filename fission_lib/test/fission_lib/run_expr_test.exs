defmodule RunExprTest do
  use ExUnit.Case, async: true
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @moduletag :tmp_dir

  async_test "run simple expression", %{tmp_dir: run_dir} do
    info =
      quote do
        args.n + 3
      end
      |> AtomVM.compile_quoted()
      |> AtomVM.run(run_dir, n: 2)

    assert 5 = info.result
  end
end
