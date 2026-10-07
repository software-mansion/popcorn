defmodule LocalLiveView.E2E.Server do
  @moduledoc false
  # Builds what the e2e app's pages load in the browser, and serves the app
  # from the current VM. See test/e2e/README.md.

  use Supervisor

  @e2e_dir Path.expand("..", __DIR__)
  @llv_dir Path.expand("../../..", __DIR__)
  @js_dir Path.join(@e2e_dir, "priv/static/assets/js")
  @runtime_files ~w(iframe.mjs AtomVM.mjs AtomVM.wasm)

  @doc """
  Builds LocalLiveView's JS bundle, the local views' Wasm bundle and the
  app's `app.js`, from the current sources.
  """
  def build! do
    Mix.Task.run("llv.assets")

    File.mkdir_p!(Path.join(@js_dir, "wasm"))

    for file <- @runtime_files do
      File.cp!(Path.join(@llv_dir, "priv/static/#{file}"), Path.join(@js_dir, file))
    end

    {output, status} =
      System.cmd("mix", ["build"],
        cd: Path.join(@e2e_dir, "local"),
        env: [{"MIX_ENV", "dev"}],
        stderr_to_stdout: true
      )

    if status != 0, do: raise("building the Wasm bundle failed:\n#{output}")

    # The esbuild package is held at 0.8 by playwright, and that version only
    # verifies binaries npm signed before rotating its key in 2025.
    Application.put_env(:esbuild, :version, "0.24.2")
    {:ok, _apps} = Application.ensure_all_started(:esbuild)

    Application.put_env(:esbuild, :llv_e2e,
      args:
        ~w(app.js --bundle --format=esm --target=es2022 --outdir=#{@js_dir}) ++
          ["--alias:local_live_view=#{@llv_dir}/priv/static/local_live_view.js"],
      cd: Path.join(@e2e_dir, "assets"),
      env: %{"NODE_PATH" => Path.join(@llv_dir, "deps")}
    )

    0 = Esbuild.install_and_run(:llv_e2e, [])
    :ok
  end

  def start_link(port) do
    Supervisor.start_link(__MODULE__, port, name: __MODULE__)
  end

  @impl true
  def init(port) do
    Application.put_env(:local_live_view, LocalLiveView.E2E.Endpoint,
      adapter: Bandit.PhoenixAdapter,
      server: true,
      http: [ip: {127, 0, 0, 1}, port: port],
      url: [host: "localhost", port: port],
      check_origin: false,
      render_errors: [formats: [html: LocalLiveView.E2E.ErrorHTML], layout: false],
      pubsub_server: LocalLiveView.E2E.PubSub,
      live_view: [signing_salt: "llv-e2e-signing-salt"],
      secret_key_base: String.duplicate("llv-e2e-secret-key-base", 3)
    )

    {:ok, _apps} = Application.ensure_all_started([:phoenix, :phoenix_live_view, :bandit])

    Supervisor.init(
      [{Phoenix.PubSub, name: LocalLiveView.E2E.PubSub}, LocalLiveView.E2E.Endpoint],
      strategy: :one_for_one
    )
  end
end
