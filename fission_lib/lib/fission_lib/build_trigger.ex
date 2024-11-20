defmodule FissionLib.BuildTrigger do
  @moduledoc false
  # Triggers the patches compilation. May hang LSP ¯\_(ツ)_/¯

  @patches "patches/**/*"
  paths = Path.wildcard(@patches)
  @paths_hash :erlang.md5(paths)

  for path <- paths do
    @external_resource path
  end

  def __mix_recompile__?() do
    Path.wildcard(@patches) |> :erlang.md5() != @paths_hash
  end

  out_dir = Mix.Project.build_path()
  config = FissionLib.Config.get()

  FissionLib.Build.build(
    out_dir,
    config.ex_stdlib_beam_paths ++ config.erl_stdlib_beam_paths,
    config.add_tracing
  )
end
