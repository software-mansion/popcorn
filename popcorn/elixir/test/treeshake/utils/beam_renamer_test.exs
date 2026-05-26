defmodule Treeshake.Utils.BeamRenamerTest do
  use ExUnit.Case, async: true

  alias Treeshake.Utils.BeamRenamer

  @moduletag :treeshake

  @ebin "test/fixtures/treeshake/demo_app/_build/prod/lib/demo_app/ebin"

  defp beam(module), do: Path.join(@ebin, "#{module}.beam") |> String.to_charlist()

  defp make_tmp_dir do
    dir = Path.join(System.tmp_dir!(), "beam_renamer_#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    dir
  end

  defp write_renamed(source, new_mod, out) do
    binary = BeamRenamer.rename(source, new_mod)
    path = Path.join(out, "#{new_mod}.beam")
    File.write!(path, binary)
    path
  end

  describe "rename/2 - output binary" do
    test "returns binary with new module name" do
      binary = BeamRenamer.rename(beam("Elixir.DemoApp.Worker"), :"Elixir.DemoApp.Renamed")
      {:ok, mod, _chunks} = :beam_lib.all_chunks(binary)
      assert mod == :"Elixir.DemoApp.Renamed"
    end

    test "returned binary does not report old module name" do
      binary = BeamRenamer.rename(beam("Elixir.DemoApp.Worker"), :"Elixir.DemoApp.Renamed")
      {:ok, mod, _chunks} = :beam_lib.all_chunks(binary)
      refute mod == :"Elixir.DemoApp.Worker"
    end
  end

  describe "rename/2 - BEAM validity" do
    test "output is a valid BEAM (all_chunks succeeds)" do
      binary = BeamRenamer.rename(beam("Elixir.DemoApp.Worker"), :"Elixir.DemoApp.ValidCheck")

      assert {:ok, :"Elixir.DemoApp.ValidCheck", _chunks} = :beam_lib.all_chunks(binary)
    end

    test "output is loadable by the runtime" do
      mod = :"Elixir.DemoApp.Loadable"

      on_exit(fn ->
        :code.purge(mod)
        :code.delete(mod)
      end)

      binary = BeamRenamer.rename(beam("Elixir.DemoApp.Worker"), mod)

      assert {:module, ^mod} = :code.load_binary(mod, ~c"Elixir.DemoApp.Loadable.beam", binary)
    end

    test "module_info/0 reports the new module name after loading" do
      mod = :"Elixir.DemoApp.ModuleInfoCheck"

      on_exit(fn ->
        :code.purge(mod)
        :code.delete(mod)
      end)

      binary = BeamRenamer.rename(beam("Elixir.DemoApp.Worker"), mod)
      :code.load_binary(mod, ~c"Elixir.DemoApp.ModuleInfoCheck.beam", binary)

      assert mod.module_info(:module) == mod
    end

    test "chunks are preserved from original" do
      {:ok, _old_mod, old_chunks} = :beam_lib.all_chunks(beam("Elixir.DemoApp.Worker"))

      binary = BeamRenamer.rename(beam("Elixir.DemoApp.Worker"), :"Elixir.DemoApp.ChunkCheck")
      {:ok, _, new_chunks} = :beam_lib.all_chunks(binary)

      old_names = Enum.map(old_chunks, &elem(&1, 0)) |> Enum.sort()
      new_names = Enum.map(new_chunks, &elem(&1, 0)) |> Enum.sort()

      assert old_names == new_names
    end
  end

  describe "rename/2 - empty module from Code.compile_quoted" do
    test "renames the module and module_info/1 returns the new name" do
      src_mod = :"Elixir.BeamRenamerTest.EmptySource"
      new_mod = :"Elixir.BeamRenamerTest.EmptyRenamed"

      [{^src_mod, beam_binary}] =
        quote do
          defmodule unquote(src_mod) do
          end
        end
        |> Code.compile_quoted()

      on_exit(fn ->
        :code.purge(src_mod)
        :code.delete(src_mod)
        :code.purge(new_mod)
        :code.delete(new_mod)
      end)

      binary = BeamRenamer.rename(beam_binary, new_mod)

      {:ok, ^new_mod, _} = :beam_lib.all_chunks(binary)
      :code.load_binary(new_mod, ~c"Elixir.BeamRenamerTest.EmptyRenamed.beam", binary)
      assert new_mod.module_info(:module) == new_mod
    end
  end

  describe "rename/2 - different source modules" do
    test "renames DemoApp.Application" do
      binary =
        BeamRenamer.rename(beam("Elixir.DemoApp.Application"), :"Elixir.DemoApp.RenamedApp")

      {:ok, mod, _} = :beam_lib.all_chunks(binary)
      assert mod == :"Elixir.DemoApp.RenamedApp"
    end
  end

  describe "rename/2 - error cases" do
    test "raises when input file does not exist" do
      assert_raise MatchError, fn ->
        BeamRenamer.rename(~c"/tmp/no_such_file_for_renamer.beam", :"Elixir.Foo")
      end
    end
  end

  describe "write_renamed helper - file output" do
    test "writes a file named after the new module" do
      out = make_tmp_dir()
      on_exit(fn -> File.rm_rf!(out) end)

      write_renamed(beam("Elixir.DemoApp.Worker"), :"Elixir.DemoApp.Renamed", out)

      assert File.exists?(Path.join(out, "Elixir.DemoApp.Renamed.beam"))
    end

    test "does not write a file with the old module name" do
      out = make_tmp_dir()
      on_exit(fn -> File.rm_rf!(out) end)

      write_renamed(beam("Elixir.DemoApp.Worker"), :"Elixir.DemoApp.Renamed", out)

      refute File.exists?(Path.join(out, "Elixir.DemoApp.Worker.beam"))
    end
  end
end
