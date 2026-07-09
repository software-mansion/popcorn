defmodule LocalLvKanban.E2ETest do
  @moduledoc """
  Runs the Playwright end-to-end suite (`test/playwright`) from ExUnit.

  Tagged `:e2e` and slow — it needs built assets (`mix llv.build` + `mix assets.build`)
  and a free port 4901. Run it with:

      mix test               # unit + e2e
      mix test --only e2e    # just this suite
      mix test --exclude e2e # fast: unit tests only

  Unlike a typical Playwright setup, the suite does NOT launch its own web server.
  This test starts the Phoenix endpoint **in the same BEAM that runs the test**
  (MIX_ENV=test) on :4901, then drives Playwright against it:

    * The Repo's SQL sandbox is put in shared mode so the out-of-process Bandit /
      LiveView request processes the browser spins up share this test's connection —
      everything the suite writes rolls back when the test finishes.
    * The endpoint (`server: false` in test) is flipped to serving by restarting its
      supervised child with `server: true`; this wires the Bandit adapter with full
      `/live` + `/llv_socket` websocket support. It's restored to `server: false` on
      exit so it doesn't bind :4901 outside this test.

  `playwright.config.js` only points at http://localhost:4901 — it no longer owns a
  server. The suite is a pnpm-workspace package; this shells out to `pnpm test`.
  """
  use ExUnit.Case, async: false

  alias Ecto.Adapters.SQL.Sandbox

  @endpoint LocalLvKanbanWeb.Endpoint
  @otp_app :local_lv_kanban
  @supervisor LocalLvKanban.Supervisor

  @moduletag :e2e
  # Includes the Playwright suite plus a possible one-off asset build.
  @moduletag timeout: :timer.minutes(15)

  @app_root Path.expand("..", __DIR__)
  @playwright_dir Path.join(__DIR__, "playwright")
  @port 4901

  setup_all do
    ensure_node_deps!()
    ensure_assets!()

    # Shared mode lets the browser-driven Bandit/LiveView processes (which aren't
    # descendants of the test) use this connection; the whole run rolls back after.
    owner = Sandbox.start_owner!(LocalLvKanban.Repo, shared: true)
    on_exit(fn -> Sandbox.stop_owner(owner) end)

    start_server!()

    :ok
  end

  test "playwright e2e suite passes" do
    {_output, status} =
      System.cmd("pnpm", ["test"],
        cd: @playwright_dir,
        stderr_to_stdout: true,
        into: IO.stream()
      )

    assert status == 0, "Playwright e2e suite failed (exit status #{status})"
  end

  # -- in-VM server lifecycle -------------------------------------------------

  defp start_server! do
    original = Application.get_env(@otp_app, @endpoint)

    serving =
      Keyword.merge(original,
        server: true,
        http: [ip: {127, 0, 0, 1}, port: @port],
        url: [host: "localhost", port: @port]
      )

    Application.put_env(@otp_app, @endpoint, serving)
    restart_endpoint!()
    wait_until_up!()

    # Restore the non-serving endpoint so later (unit) tests and the endpoint's own
    # config cache keep working without :4901 bound.
    on_exit(fn ->
      Application.put_env(@otp_app, @endpoint, original)
      restart_endpoint!()
    end)
  end

  defp restart_endpoint! do
    :ok = Supervisor.terminate_child(@supervisor, @endpoint)
    {:ok, _} = Supervisor.restart_child(@supervisor, @endpoint)
    :ok
  end

  defp wait_until_up! do
    wait_until_up!(System.monotonic_time(:millisecond) + 30_000)
  end

  defp wait_until_up!(deadline) do
    cond do
      listening?() ->
        :ok

      System.monotonic_time(:millisecond) > deadline ->
        raise "endpoint did not start serving on :#{@port} within 30s"

      true ->
        Process.sleep(200)
        wait_until_up!(deadline)
    end
  end

  defp listening? do
    case :gen_tcp.connect(~c"localhost", @port, [:binary, active: false], 500) do
      {:ok, sock} ->
        :gen_tcp.close(sock)
        true

      {:error, _} ->
        false
    end
  end

  # -- prerequisites ----------------------------------------------------------

  # The dev server used to build app.js/app.css on boot; in test env esbuild/tailwind
  # are `runtime: false`, so build them up front (in :dev) when missing. `mix llv.build`
  # in the `test` alias already produced the WASM bundle + AtomVM.*; this fills the gap.
  defp ensure_assets! do
    unless File.regular?(Path.join(@app_root, "priv/static/assets/js/app.js")) do
      {_out, 0} =
        System.cmd("mix", ["assets.setup"],
          cd: @app_root,
          env: [{"MIX_ENV", "dev"}],
          into: IO.stream(),
          stderr_to_stdout: true
        )

      {_out, 0} =
        System.cmd("mix", ["assets.build"],
          cd: @app_root,
          env: [{"MIX_ENV", "dev"}],
          into: IO.stream(),
          stderr_to_stdout: true
        )
    end
  end

  defp ensure_node_deps! do
    unless File.dir?(Path.join(@playwright_dir, "node_modules/@playwright/test")) do
      {_out, 0} = System.cmd("pnpm", ["install"], cd: @playwright_dir, stderr_to_stdout: true)
    end
  end
end
