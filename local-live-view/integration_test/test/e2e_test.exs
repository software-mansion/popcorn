defmodule LlvIntegration.E2ETest do
  @moduledoc """
  Runs the Playwright suite (`test/playwright`) against this app.

  The suite doesn't start a server: this test serves the endpoint from the
  test VM on :4904, by restarting it with `server: true`, runs the suite, and
  restores the endpoint afterwards. The `test` alias rebuilds the Wasm bundle
  and `app.js` first, so the suite runs against the current local_live_view.
  """
  use ExUnit.Case, async: false

  @endpoint LlvIntegrationWeb.Endpoint
  @otp_app :llv_integration
  @supervisor LlvIntegration.Supervisor
  @playwright_dir Path.join(__DIR__, "playwright")
  @port 4904

  @moduletag timeout: :timer.minutes(15)

  setup_all do
    unless File.dir?(Path.join(@playwright_dir, "node_modules/@playwright/test")) do
      {_output, 0} = System.cmd("pnpm", ["install"], cd: @playwright_dir, stderr_to_stdout: true)
    end

    start_server!()
    :ok
  end

  test "playwright suite passes" do
    {_output, status} =
      System.cmd("pnpm", ["test"],
        cd: @playwright_dir,
        env: [{"BASE_URL", "http://localhost:#{@port}"}],
        stderr_to_stdout: true,
        into: IO.stream()
      )

    assert status == 0, "Playwright suite failed (exit status #{status})"
  end

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
    wait_until_up!(System.monotonic_time(:millisecond) + 30_000)

    on_exit(fn ->
      Application.put_env(@otp_app, @endpoint, original)
      restart_endpoint!()
    end)
  end

  defp restart_endpoint! do
    :ok = Supervisor.terminate_child(@supervisor, @endpoint)
    {:ok, _pid} = Supervisor.restart_child(@supervisor, @endpoint)
    :ok
  end

  defp wait_until_up!(deadline) do
    case :gen_tcp.connect(~c"localhost", @port, [:binary, active: false], 500) do
      {:ok, socket} ->
        :gen_tcp.close(socket)

      {:error, _reason} ->
        if System.monotonic_time(:millisecond) > deadline do
          raise "endpoint did not start serving on :#{@port} within 30s"
        end

        Process.sleep(200)
        wait_until_up!(deadline)
    end
  end
end
