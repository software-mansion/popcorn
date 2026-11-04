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
      # Mix for Mix.install/2, and Leex and Yecc for the Erlang packages it compiles
      extra_applications: [:logger, :mix, :parsetools, :ex_unit],
      mod: {LivebookWasm.Application, []}
    ]
  end

  # Livebook is started by LivebookWasm.Application, once its stand-ins run.
  # Notebooks can't Mix.install, so Kino is in the VM for them.
  defp deps do
    [
      {:popcorn, path: "../../popcorn/elixir"},
      {:livebook, "~> 0.19.10", runtime: false},
      {:kino, "~> 0.18.0"},
      # For Popcorn.Plug's WebSockets. Livebook's Phoenix 1.8.7 requires ~> 0.5.3.
      {:websock_adapter, "~> 0.7.1", override: true}
    ]
  end

  defp aliases do
    ["popcorn.dev": [&copy_livebook_static/1, &copy_hex/1, "popcorn.dev"]]
  end

  # Hex is a Mix archive rather than a dependency. Popcorn packages the build's
  # applications, so it's copied among them for Mix.install/2 in the VM.
  defp copy_hex(_args) do
    ebin =
      Mix.path_for(:archives) |> Path.join("hex-*/hex-*/ebin") |> Path.wildcard() |> Enum.max()

    app_dir = Path.join(Mix.Project.build_path(), "lib/hex")
    File.mkdir_p!(app_dir)
    File.cp_r!(ebin, Path.join(app_dir, "ebin"))
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
