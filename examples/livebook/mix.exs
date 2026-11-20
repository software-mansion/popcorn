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
      # Supports LivebookWasm.Plug's WebSockets from 0.7.1. Livebook's Phoenix
      # 1.8.7 requires ~> 0.5.3.
      {:websock_adapter, "~> 0.7.1", override: true}
    ]
  end

  defp aliases do
    [
      "popcorn.dev": [&copy_livebook_static/1, &cook_notebook_vm/1, "popcorn.dev"],
      # Builds public/ as `mix popcorn.dev` does, for a static host
      cook: [&copy_livebook_static/1, &cook_notebook_vm/1, "popcorn.cook"],
      # The application starts Livebook, which the tests don't need
      test: "test --no-start"
    ]
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
    static = "deps/livebook/priv/static"

    for dir <- ~w(assets images favicons iframe) do
      File.cp_r!(Path.join(static, dir), Path.join("public", dir))
    end

    # Livebook has many of its files only gzipped, for Plug.Static's `gzip`.
    # Static hosts serve files that are there as they are, see
    # docker/livebook.nginx.conf.
    for dir <- ~w(assets images favicons iframe),
        gzipped <- Path.wildcard(Path.join([static, dir, "**/*.gz"])),
        file = Path.rootname(gzipped),
        not File.exists?(file) do
      target = Path.join("public", Path.relative_to(file, static))
      File.write!(target, gzipped |> File.read!() |> :zlib.gunzip())
    end
  end
end
