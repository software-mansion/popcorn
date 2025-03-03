defmodule FissionLib.AppTest do
  use ExUnit.Case, async: true

  require FissionLib.Support.AtomVM, as: AtomVM

  @moduletag :tmp_dir

  @tag :app
  test "app", %{tmp_dir: tmp_dir} do
    quote do
      :application_controller.start(:kernel)
      :application.ensure_all_started(:elixir)
      :ok
    end
    |> AtomVM.compile_quoted()
    |> AtomVM.run(tmp_dir)
    |> AtomVM.assert_result(:ok)
  end
end
