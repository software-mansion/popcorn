defmodule FissionLib.ElixirModuleTest do
  use ExUnit.Case, async: true
  require Logger
  alias FissionLib.AtomVM

  @moduletag :tmp_dir
  @examples_path "./test/examples"
  setup_all do
    quote do
      :elixir.start([], [])

      var!(code)
      |> :erlang.binary_to_list()
      |> Code.eval_string([], __ENV__)
      |> elem(0)
    end
    |> AtomVM.compile("tmp_mod_elixir", [:code])

    :ok
  end

  defp run(code, tmp_dir) do
    assert {:module, _name, _bin, _} = AtomVM.run("tmp_mod_elixir", tmp_dir, code: code)
  end

  test "simple_module", %{tmp_dir: tmp_dir} do
    """
    defmodule Start do

    end
    """
    |> run(tmp_dir)
  end

  test "CapybaraHabitat", %{tmp_dir: tmp_dir} do
    File.read!("#{@examples_path}/CapybaraHabitat.ex")
    |> run(tmp_dir)
  end
end
