defmodule Treeshake.Utils.BeamRewriterTest do
  use ExUnit.Case, async: true

  alias Treeshake.Utils.BeamRewriter

  @moduletag :treeshake

  @ebin "test/fixtures/treeshake/demo_app/_build/prod/lib/demo_app/ebin"

  defp beam(module), do: Path.join(@ebin, "#{module}.beam")

  # Reads all function entries from the compiled bytecode via beam_disasm.
  # Works regardless of debug_info format, including beams compiled from core erlang.
  defp all_funs(binary) when is_binary(binary) do
    tmp = Path.join(System.tmp_dir!(), "bf#{:erlang.unique_integer([:positive])}.beam")
    File.write!(tmp, binary)

    try do
      case :beam_disasm.file(String.to_charlist(tmp)) do
        {:beam_file, _, _, _, _, code} ->
          for {:function, name, arity, _, _} <- code, do: {name, arity}

        {:error, :beam_disasm, _} ->
          []
      end
    after
      File.rm(tmp)
    end
  end

  defp beam_exports(binary) when is_binary(binary) do
    {:ok, {_mod, [exports: exports]}} = :beam_lib.chunks(binary, [:exports])
    exports
  end

  # ---------------------------------------------------------------------------
  # Happy-path
  # ---------------------------------------------------------------------------

  # process/1 calls upcase/1 and wrap/1 — the caller is responsible for
  # passing the full set including local dependencies.
  @worker_process_funs [process: 1, upcase: 1, wrap: 1]

  describe "keep_funs/2 - basic" do
    test "returns {binary, removed}" do
      assert {binary, removed} =
               BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), @worker_process_funs)

      assert is_binary(binary)
      assert is_list(removed)
    end

    test "kept function is present in abstract code" do
      {binary, _} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), @worker_process_funs)
      assert {:process, 1} in all_funs(binary)
    end

    test "non-kept function is absent from abstract code" do
      {binary, _} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), @worker_process_funs)
      refute {:unused, 1} in all_funs(binary)
    end

    test "kept function is present in exports" do
      {binary, _} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), @worker_process_funs)
      assert {:process, 1} in beam_exports(binary)
    end

    test "non-kept function is absent from exports" do
      {binary, _} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), @worker_process_funs)
      refute {:unused, 1} in beam_exports(binary)
    end

    test "removed list contains the stripped function" do
      {_, removed} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), @worker_process_funs)
      assert {:unused, 1} in removed
    end

    test "removed list does not contain kept functions" do
      {_, removed} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), @worker_process_funs)
      refute {:process, 1} in removed
    end
  end

  describe "keep_funs/2 - multiple functions" do
    test "all listed functions are present" do
      {binary, _} =
        BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"),
          process: 1,
          unused: 1,
          upcase: 1,
          wrap: 1
        )

      funs = all_funs(binary)
      assert {:process, 1} in funs
      assert {:unused, 1} in funs
    end

    test "none of the explicitly kept functions appear in removed" do
      keep = [process: 1, unused: 1, upcase: 1, wrap: 1]
      {_, removed} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), keep)
      for fa <- keep, do: refute(fa in removed)
    end
  end

  describe "keep_funs/2 - empty list" do
    test "returns binary with no user-defined functions and all as removed" do
      {binary, removed} = BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), [])
      assert is_binary(binary)
      assert all_funs(binary) == []
      assert {:unused, 1} in removed
      assert {:process, 1} in removed
    end
  end

  describe "keep_funs/2 - function not present" do
    test "silently ignores names not in the module" do
      {binary, _removed} =
        BeamRewriter.keep_funs(beam("Elixir.DemoApp.Worker"), totally_absent: 99)

      assert is_binary(binary)
    end
  end

  # ---------------------------------------------------------------------------
  # Error cases
  # ---------------------------------------------------------------------------

  describe "keep_funs/2 - errors" do
    test "raises for a non-existent file" do
      assert_raise MatchError, fn ->
        BeamRewriter.keep_funs("/tmp/no_such_file_at_all.beam", foo: 1)
      end
    end

    test "raises for a file with no debug_info" do
      forms = [
        {:attribute, 1, :module, :NoDebugInfoMod},
        {:attribute, 2, :export, [{:hello, 0}]},
        {:function, 3, :hello, 0, [{:clause, 3, [], [], [{:atom, 3, :ok}]}]}
      ]

      {:ok, :NoDebugInfoMod, binary, _} =
        :compile.forms(forms, [:return_errors, :return_warnings])

      tmp =
        Path.join(System.tmp_dir!(), "no_debug_info_#{:erlang.unique_integer([:positive])}.beam")

      File.write!(tmp, binary)
      on_exit(fn -> File.rm(tmp) end)

      assert_raise MatchError, fn ->
        BeamRewriter.keep_funs(tmp, hello: 0)
      end
    end
  end
end
