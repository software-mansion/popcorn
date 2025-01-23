defmodule FissionLib.ElixirModuleTest do
  use ExUnit.Case, async: true
  require Logger

  @moduletag :tmp_dir
  @path_to_examples "test/examples"
  setup_all do
    quote do
      code = var!(code) |> :erlang.binary_to_list()
      :elixir.start([], [])
      case Code.eval_string(code, [], __ENV__) do
        {result, _binding} -> result
        other -> other
      end

    end
    |> RunInAtomVM.compile("tmp_mod_elixir", [:code])

    :ok
  end

  defp run(code, tmp_dir) do
    assert {:module, _name, _bin, _} = RunInAtomVM.run("tmp_mod_elixir", tmp_dir, code: code)
  end

  defp assert_ok(x), do: assert({:module, _name, _bin, _} = x)

  test "simple_module", %{tmp_dir: tmp_dir} do
    """
    defmodule Start do
    end
    """
    |> run(tmp_dir)
    |> assert_ok()
  end

  test "CapybaraHabitat", %{tmp_dir: tmp_dir} do
    File.read!(
      "#{@path_to_examples}/CapybaraHabitat.ex"
    )
    |> run(tmp_dir)
    |> assert_ok()
  end
end
