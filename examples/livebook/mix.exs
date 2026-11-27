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
      0 -> pack_notebook_vm_docs("public/notebook_vm")
      status -> Mix.raise("Cooking notebook_vm/ failed with status #{status}")
    end
  end

  # Elixir's and Erlang's docs, for Livebook's code completion and docs in the
  # notebooks. Popcorn's package leaves them out, so they're in a tar of their
  # own, which the VMs load after boot: see NotebookVM.Docs. They're the host's
  # docs of the modules in the package's Elixir and OTP applications, as chunk
  # files, where Code.fetch_docs/1 looks for docs that `.beam` files don't have.
  # Hosts serve the tar's gzipped copy, which the browser decompresses.
  defp pack_notebook_vm_docs(dir) do
    manifest = Path.join(dir, "otp/manifest.json") |> File.read!() |> JSON.decode!()
    stdlib = Path.join([dir, "otp", manifest["bundles"]["stdlib"]["tar"]])
    {:ok, files} = :erl_tar.table(String.to_charlist(stdlib))

    chunks =
      for path <- files,
          ["lib", app, "ebin", file] <- [Path.split(to_string(path))],
          Path.extname(file) == ".beam",
          module = Path.rootname(file),
          {:docs_v1, _, _, _, _, _, _} = docs <- [Code.fetch_docs(String.to_atom(module))] do
        chunk = :erlang.term_to_binary(docs, [:deterministic])
        {~c"lib/#{app}/doc/chunks/#{module}.chunk", chunk}
      end

    # The same docs make the same files, which browsers have cached
    tar = Path.join(dir, "docs.tar")
    opts = [mtime: 0, atime: 0, ctime: 0, uid: 0, gid: 0]
    :ok = :erl_tar.create(String.to_charlist(tar), chunks, opts)
    File.write!(tar <> ".gz", :zlib.gzip(File.read!(tar)))
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
