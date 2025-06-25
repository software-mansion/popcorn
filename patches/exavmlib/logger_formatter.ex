import Kernel, except: [inspect: 2]

defmodule Logger.Formatter do
  @compile {:popcorn_patch_private, [translator_inspect_opts: 0]}

  # Patch reason: logger application is not started in AtomVM
  def translator_inspect_opts() do
    # Application.fetch_env!(:logger, :translator_inspect_opts)
    []
  end
end
