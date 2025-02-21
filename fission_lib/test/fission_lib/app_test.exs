defmodule FissionLib.AppTest do
  use ExUnit.Case, async: true

  alias FissionLib.Support.AtomVM

  @moduletag :tmp_dir

  @tag :app
  test "app", %{tmp_dir: tmp_dir} do
    info =
      quote do
        :application_controller.start(:kernel)
        :application.ensure_all_started(:elixir)
        :ok
      end
      |> AtomVM.compile_quoted()
      |> AtomVM.run(tmp_dir)

    assert info.result == :ok
  end
end
