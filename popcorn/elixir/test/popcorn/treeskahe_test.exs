defmodule Popcorn.TreeshakeTest do
  use ExUnit.Case, async: true

  import AsyncTest
  require Popcorn.Support.AtomVM, as: AtomVM

  @moduletag :tmp_dir

  async_test ":lists.duplicate/2", %{tmp_dir: tmp_dir} do
    quote do
      :lists.duplicate(0, "a")
    end
    |> AtomVM.compile_quoted(treeshake: true)
    |> AtomVM.run(tmp_dir)
    |> AtomVM.assert_result([])
  end
end
