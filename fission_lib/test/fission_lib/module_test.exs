defmodule FissionLib.ModuleTest do
  use ExUnit.Case, async: true
  require Logger

  @moduletag :tmp_dir
  setup_all do
    quote do
      code = var!(code) |> :erlang.binary_to_list()

      try do
        compile_opts = [
          :deterministic,
          :return_errors,
          :compressed,
          :no_spawn_compiler_process,
          :no_docs
        ]

        split_forms = fn forms ->
          split_on_dots = fn
            {:dot, _} = f, current -> {:cont, Enum.reverse([f | current]), []}
            f, current -> {:cont, [f | current]}
          end

          ensure_empty_acc = fn [] -> {:cont, []} end

          Enum.chunk_while(forms, [], split_on_dots, ensure_empty_acc)
        end

        parse_form = fn form_tok ->
          {:ok, form} = :erl_parse.parse_form(form_tok)
          form
        end

        with {:ok, tokens, _end_location} <- :erl_scan.string(code),
             {:ok, module, module_bin} <-
               tokens
               |> split_forms.()
               |> Enum.map(parse_form)
               |> :compile.noenv_forms(compile_opts),
             {:module, _module} <- :code.load_binary(module, ~c"nofile", module_bin) do
          :ok
        end
      rescue
        error -> {:error, error, __STACKTRACE__}
      end
    end
    |> RunInAtomVM.compile("tmp", [:code])

    :ok
  end

  defp run(code, tmp_dir) do
    RunInAtomVM.run("tmp", tmp_dir, code: code)
  end

  defp assert_ok(x), do: assert(x == :ok)

  @tag :erl_mod
  test "module", %{tmp_dir: tmp_dir} do
    {time, _result} =
      :timer.tc(fn ->
        """
        -module(foo).
        -export([a1/0, a2/0]).
        a1() -> {a, b, c, d, e, f}.
        a2() -> {a, b, c, d, e, f}.
        """
        |> run(tmp_dir)
        |> assert_ok()
      end)

    IO.puts(time)
  end

  @tag :erl_mod_long
  test "long module", %{tmp_dir: tmp_dir} do
    {time, _result} =
      :timer.tc(fn ->
        """
        -module(foo).
        -export([a1/0,
        a2/0,
        a3/0,
        a4/0,
        a5/0,
        a6/0,
        a7/0,
        a8/0,
        a9/0,
        a10/0,
        a11/0,
        a12/0,
        a13/0,
        a14/0,
        a15/0,
        a16/0,
        a17/0,
        a18/0,
        a19/0,
        a20/0,
        a21/0,
        a22/0,
        a23/0,
        a24/0,
        a25/0,
        a26/0,
        a27/0,
        a28/0,
        a29/0,
        a30/0,
        a31/0,
        a32/0,
        a33/0,
        a34/0,
        a35/0,
        a36/0,
        a37/0,
        a38/0,
        a39/0,
        a40/0,
        a41/0,
        a42/0,
        a43/0,
        a44/0,
        a45/0,
        a46/0,
        a47/0,
        a48/0,
        a49/0,
        a50/0]).
        a1() -> {a, b, c, d, e, f}.
        a2() -> {a, b, c, d, e, f}.
        a3() -> {a, b, c, d, e, f}.
        a4() -> {a, b, c, d, e, f}.
        a5() -> {a, b, c, d, e, f}.
        a6() -> {a, b, c, d, e, f}.
        a7() -> {a, b, c, d, e, f}.
        a8() -> {a, b, c, d, e, f}.
        a9() -> {a, b, c, d, e, f}.
        a10() -> {a, b, c, d, e, f}.
        a11() -> {a, b, c, d, e, f}.
        a12() -> {a, b, c, d, e, f}.
        a13() -> {a, b, c, d, e, f}.
        a14() -> {a, b, c, d, e, f}.
        a15() -> {a, b, c, d, e, f}.
        a16() -> {a, b, c, d, e, f}.
        a17() -> {a, b, c, d, e, f}.
        a18() -> {a, b, c, d, e, f}.
        a19() -> {a, b, c, d, e, f}.
        a20() -> {a, b, c, d, e, f}.
        a21() -> {a, b, c, d, e, f}.
        a22() -> {a, b, c, d, e, f}.
        a23() -> {a, b, c, d, e, f}.
        a24() -> {a, b, c, d, e, f}.
        a25() -> {a, b, c, d, e, f}.
        a26() -> {a, b, c, d, e, f}.
        a27() -> {a, b, c, d, e, f}.
        a28() -> {a, b, c, d, e, f}.
        a29() -> {a, b, c, d, e, f}.
        a30() -> {a, b, c, d, e, f}.
        a31() -> {a, b, c, d, e, f}.
        a32() -> {a, b, c, d, e, f}.
        a33() -> {a, b, c, d, e, f}.
        a34() -> {a, b, c, d, e, f}.
        a35() -> {a, b, c, d, e, f}.
        a36() -> {a, b, c, d, e, f}.
        a37() -> {a, b, c, d, e, f}.
        a38() -> {a, b, c, d, e, f}.
        a39() -> {a, b, c, d, e, f}.
        a40() -> {a, b, c, d, e, f}.
        a41() -> {a, b, c, d, e, f}.
        a42() -> {a, b, c, d, e, f}.
        a43() -> {a, b, c, d, e, f}.
        a44() -> {a, b, c, d, e, f}.
        a45() -> {a, b, c, d, e, f}.
        a46() -> {a, b, c, d, e, f}.
        a47() -> {a, b, c, d, e, f}.
        a48() -> {a, b, c, d, e, f}.
        a49() -> {a, b, c, d, e, f}.
        a50() -> {a, b, c, d, e, f}.
        """
        |> run(tmp_dir)
        |> assert_ok()
      end)

    IO.puts(time)
  end
end
