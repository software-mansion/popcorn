defmodule FissionLib.ErlangModuleTest do
  use ExUnit.Case, async: true
  require Logger

  @moduletag :tmp_dir
  @path_to_examples "test/examples"
  setup_all do
    quote do
      code = var!(code) |> :erlang.binary_to_list()

      parse_form = fn form_tok ->
        {:ok, form} = :erl_parse.parse_form(form_tok)
        form
      end

      split_on_dots = fn
        {:dot, _} = f, current -> {:cont, Enum.reverse([f | current]), []}
        f, current -> {:cont, [f | current]}
      end

      ensure_empty_acc = fn [] -> {:cont, []} end

      {:ok, tokens, _} = :erl_scan.string(code)
      IO.puts("Scanned")

      {:ok, module, module_bin} =
        Enum.chunk_while(tokens, [], split_on_dots, ensure_empty_acc)
        |> Enum.map(parse_form)
        |> :compile.noenv_forms([
          :deterministic,
          :return_errors,
          :compressed,
          :no_spawn_compiler_process,
          :no_docs
        ])

      IO.puts("compiled")

      :code.load_binary(module, ~c"nofile", module_bin)
    end
    |> RunInAtomVM.compile("tmp_mod_erl", [:code])

    :ok
  end

  defp run(code, tmp_dir) do
    assert {:module, _} = RunInAtomVM.run("tmp_mod_erl", tmp_dir, code: code)
  end

  defp assert_ok(x), do: assert({:module, _} = x)

  test "simple_module", %{tmp_dir: tmp_dir} do
    """
    -module(test).
    -export([add/2]).

    add(A, B) ->
        A + B.
    """
    |> run(tmp_dir)
    |> assert_ok()
  end

  test "simple_module_from_file", %{tmp_dir: tmp_dir} do
    File.read!(
      "#{@path_to_examples}/example.erl"
    )
    |> run(tmp_dir)
    |> assert_ok()
  end

  test "uuid", %{tmp_dir: tmp_dir} do
    File.read!(
      "#{@path_to_examples}/uuid.erl"
    )
    |> run(tmp_dir)
    |> assert_ok()
  end

  test "capybara_habitat" ,%{tmp_dir: tmp_dir} do
    File.read!(
      "#{@path_to_examples}/capybara_habitat.erl"
    )
    |> run(tmp_dir)
    |> assert_ok()
  end
end
