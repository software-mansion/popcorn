defmodule LocalLiveView.E2ETest do
  @moduledoc """
  Runs the Playwright suite in `test/e2e/playwright` against the e2e app,
  served from this VM. See test/e2e/README.md.
  """
  use ExUnit.Case, async: false

  alias LocalLiveView.E2E.Server

  @moduletag :e2e
  @moduletag timeout: :timer.minutes(15)

  @playwright_dir Path.join(__DIR__, "playwright")
  @port 4904

  setup_all do
    level = Logger.level()
    Logger.configure(level: :warning)
    on_exit(fn -> Logger.configure(level: level) end)

    unless File.dir?(Path.join(@playwright_dir, "node_modules/@playwright/test")) do
      {_output, 0} = System.cmd("pnpm", ["install"], cd: @playwright_dir, stderr_to_stdout: true)
    end

    Server.build!()
    start_supervised!({Server, @port})
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
end
