defmodule NotebookVM.MixInstall do
  @moduledoc """
  Sets the VM up for `Mix.install/2`.

  Hex downloads packages with `:httpc`, which Popcorn makes request with the
  browser's fetch, from jsDelivr's mirror of hex.pm: unlike hex.pm, it lets
  pages from other origins fetch. Hex checks the registry's signatures and the
  packages' checksums, so a mirror can't alter them.
  """

  @mirror "https://cdn.jsdelivr.net/hex"

  def setup do
    {:ok, _apps} = Application.ensure_all_started(:mix)
    Mix.shell(NotebookVM.MixShell)

    # Mix runs rebar3 only when it exists, and the shell compiles instead
    rebar3 = Path.join(System.tmp_dir!(), "rebar3")
    File.write!(rebar3, "")

    System.put_env(%{
      "HEX_MIRROR_URL" => @mirror,
      "MIX_REBAR3" => rebar3,
      # Mix's lock between OS processes listens on TCP. The VM is the only one
      # using its file system.
      "MIX_OS_CONCURRENCY_LOCK" => "0"
    })
  end
end
