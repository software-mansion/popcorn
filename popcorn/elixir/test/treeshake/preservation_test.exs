defmodule Treeshake.PreservationTest do
  use ExUnit.Case, async: true

  @fixture Path.expand("../fixtures/treeshake", __DIR__)

  @tag :tmp_dir
  test "keep and leave", %{tmp_dir: tmp_dir} do
    input = Path.join(tmp_dir, "input")
    output = Path.join(tmp_dir, "output")
    File.mkdir_p!(input)

    files =
      for source <- Path.wildcard(Path.join(@fixture, "*.erl")) do
        {:ok, _module} =
          :compile.file(to_charlist(source), [:debug_info, outdir: to_charlist(input)])

        Path.join(input, Path.basename(source, ".erl") <> ".beam")
      end

    preserved = Path.join(input, "preserved.beam")
    original = File.read!(preserved)

    stats =
      Treeshake.run(
        ebin_files: files,
        output_dir: output,
        keep: [:preserved],
        leave: [:preserved]
      )

    assert File.read!(Path.join(output, "preserved.beam")) == original
    assert :dead_module in stats.modules_removed

    assert {:ok, {:shake_dependency, [exports: exports]}} =
             :beam_lib.chunks(to_charlist(Path.join(output, "shake_dependency.beam")), [:exports])

    assert {:used, 0} in exports
    refute {:unused, 0} in exports
  end
end
