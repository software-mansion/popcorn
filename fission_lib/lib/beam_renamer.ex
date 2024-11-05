defmodule BeamRenamer do
  def rename_file(input, name, out_dir) do
    beam = File.read!(input)
    renamed_beam = :beam_renamer.rename(beam, :"#{name}")
    File.write!(Path.join(out_dir, "#{name}.beam"), renamed_beam)
  end
end
