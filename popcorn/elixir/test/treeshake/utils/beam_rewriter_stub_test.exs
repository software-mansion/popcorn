defmodule Treeshake.Utils.BeamRewriterStubTest do
  # async: false because tests load/unload modules into the code server
  use ExUnit.Case, async: false

  alias Treeshake.Utils.BeamRewriter

  @moduletag :treeshake

  @ebin "test/fixtures/treeshake/demo_app/_build/prod/lib/demo_app/ebin"
  @mod :"Elixir.DemoApp.Worker"

  # DemoApp.Worker public functions: process/1, unused/1
  # DemoApp.Worker private functions: upcase/1, wrap/1

  defp beam(module), do: Path.join(@ebin, "#{module}.beam")

  defp beam_exports(binary) when is_binary(binary) do
    {:ok, {_mod, [exports: exports]}} = :beam_lib.chunks(binary, [:exports])
    exports
  end

  defp load_stub_binary(binary) do
    :code.load_binary(@mod, ~c"stub_test", binary)

    on_exit(fn ->
      :code.purge(@mod)
      :code.delete(@mod)
    end)
  end

  describe "keep_funs/3 - stub_removed_public: true" do
    test "removed public functions remain exported" do
      {binary, _} =
        BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), [], stub_removed_public: true)

      exports = beam_exports(binary)
      assert {:process, 1} in exports
      assert {:unused, 1} in exports
    end

    test "removed private functions are not exported" do
      {binary, _} =
        BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), [], stub_removed_public: true)

      exports = beam_exports(binary)
      refute {:upcase, 1} in exports
      refute {:wrap, 1} in exports
    end

    test "kept functions are not replaced with stubs" do
      {binary, _} =
        BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), [process: 1, upcase: 1, wrap: 1],
          stub_removed_public: true
        )

      load_stub_binary(binary)

      # process/1 was kept — it should work normally, not raise
      assert apply(@mod, :process, ["hello"]) == "[HELLO]"
    end

    test "removed functions are included in the returned removed list" do
      {_, removed} =
        BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), [], stub_removed_public: true)

      assert {:process, 1} in removed
      assert {:unused, 1} in removed
    end

    test "without option, removed public functions are absent from exports" do
      {binary, _} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), [])
      exports = beam_exports(binary)
      refute {:process, 1} in exports
    end
  end
end
