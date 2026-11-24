defmodule NotebookVM.MixProject do
  use Mix.Project

  # The package of the notebooks' VMs, apart from Livebook's. Livebook's
  # `mix popcorn.dev` cooks it into ../public/notebook_vm.
  def project do
    [
      app: :notebook_vm,
      version: "0.1.0",
      elixir: "~> 1.20",
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      # Mix for Mix.install/2, Leex and Yecc and syntax_tools for the Erlang
      # packages it compiles, like parse_trans's parse transforms, EEx for the
      # Elixir ones that compile templates, like Ecto's Mix tasks, IEx for the
      # ones that call its helpers, like Kino, and ExUnit for Livebook's
      # doctests
      extra_applications: [:logger, :mix, :parsetools, :syntax_tools, :eex, :iex, :ex_unit],
      mod: {NotebookVM.Application, []}
    ]
  end

  defp deps do
    [{:popcorn, path: "../../../popcorn/elixir"}]
  end

  defp aliases do
    ["popcorn.cook": [&copy_hex/1, "popcorn.cook"]]
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
end
