defmodule FissionLib.AppTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  @tag :app
  test "app", %{tmp_dir: tmp_dir} do
    result =
      quote do
        :application_controller.start(:kernel)
        :application.ensure_all_started(:elixir)
        :ok
      end
      |> RunInAtomVM.expr(tmp_dir)

    assert result == :ok
  end
end
