defmodule LivebookWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :livebook_wasm,
      version: "0.1.0",
      elixir: "~> 1.20",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {LivebookWasm.Application, []}
    ]
  end

  # Livebook is started by LivebookWasm.Application, once its stand-ins run
  defp deps do
    [
      {:popcorn, path: "../../popcorn/elixir"},
      {:livebook, "~> 0.19.10", runtime: false},
      # For Popcorn.Plug's WebSockets. Livebook's Phoenix 1.8.7 requires ~> 0.5.3.
      {:websock_adapter, "~> 0.7.1", override: true}
    ]
  end

  defp aliases do
    ["popcorn.dev": [&copy_livebook_static/1, &cook_notebook_vm/1, "popcorn.dev"]]
  end

  # The notebooks' VMs boot a package of their own, which this task doesn't
  # watch: rerun it after changing notebook_vm/
  defp cook_notebook_vm(_args) do
    command = "mix do deps.get + popcorn.cook --out-dir ../public/notebook_vm"

    case Mix.shell().cmd(command, cd: "notebook_vm") do
      0 -> :ok
      status -> Mix.raise("Cooking notebook_vm/ failed with status #{status}")
    end
  end

  # The browser loads Livebook's static files from the network, not the VM
  defp copy_livebook_static(_args) do
    for dir <- ~w(assets images favicons iframe) do
      File.cp_r!(Path.join("deps/livebook/priv/static", dir), Path.join("public", dir))
    end

    # Livebook serves the outputs' iframe pages only gzipped
    for path <- Path.wildcard("public/iframe/*.html.gz") do
      File.write!(Path.rootname(path), path |> File.read!() |> :zlib.gunzip())
    end
  end
end
