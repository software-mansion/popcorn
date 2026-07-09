defmodule Mix.Tasks.Llv.InstallE2ETest do
  @moduledoc """
  End-to-end integration test for `mix llv.install`.

  Spins up a fresh Phoenix project via `mix phx.new` in a temp dir,
  injects local_live_view as a path dep, runs `mix llv.install` + `mix setup`,
  adds a `live_local "/hello_local"` route, starts `mix phx.server`, and uses Playwright
  to assert that the bundled HelloLocal component renders "Hello from WASM!" in the browser.

  Slow (~1-2 min) — covers what unit tests can't: real `mix llv.build`,
  real esbuild + popcorn.cook + AtomVM bootstrap, real DOM render.
  """

  use ExUnit.Case, async: false

  @moduletag :e2e
  # Must exceed @assertion_timeout, else ExUnit kills the test before AssertionError fires.
  @moduletag timeout: 180_000

  @port 4567
  @url "http://localhost:#{@port}"
  @server_ready_timeout 120_000
  @assertion_timeout 60_000

  setup_all do
    {:ok, _apps} = Application.ensure_all_started(:playwright)

    {_pid, browser} = Playwright.BrowserType.launch(:chromium)
    on_exit(fn -> Playwright.Browser.close(browser) end)

    project_dir = setup_phoenix_project(File.cwd!())

    on_exit(fn ->
      # Kill server + watchers before rm_rf, else watchers spin on missing files.
      kill_phoenix_subprocesses()
      Process.sleep(500)
      File.rm_rf(project_dir)
    end)

    # Reap zombie server from interrupted previous run.
    kill_port(@port)
    start_phoenix_server(project_dir)

    page = Playwright.Browser.new_page(browser)

    wait_for(
      fn -> assert %{status: 200} = Playwright.Page.goto(page, @url) end,
      @server_ready_timeout
    )

    Playwright.Page.close(page)

    [browser: browser]
  end

  test "HelloLocal renders inside the Phoenix page via live_local route", %{browser: browser} do
    page = Playwright.Browser.new_page(browser)
    Playwright.Page.goto(page, "#{@url}/hello_local")

    wait_for(
      fn ->
        text = Playwright.Page.text_content(page, "body")
        assert text =~ "Hello from WASM!"
      end,
      @assertion_timeout
    )

    Playwright.Page.close(page)
  end

  defp setup_phoenix_project(llv_path) do
    tmp_root = System.tmp_dir!() |> Path.join("llv_e2e_#{System.unique_integer([:positive])}")
    File.rm_rf!(tmp_root)
    File.mkdir_p!(tmp_root)

    run!("mix", ["phx.new", "test_app", "--no-ecto", "--no-mailer", "--install"], cd: tmp_root)

    app_dir = Path.join(tmp_root, "test_app")

    add_llv_path_dep(app_dir, llv_path)
    run!("mix", ["deps.get"], cd: app_dir)
    run!("mix", ["llv.install", "--yes"], cd: app_dir)
    patch_router(app_dir)
    run!("mix", ["setup"], cd: app_dir)

    app_dir
  end

  defp add_llv_path_dep(app_dir, llv_path) do
    mix_path = Path.join(app_dir, "mix.exs")
    content = File.read!(mix_path)

    # Insert the local_live_view dep at the top of the `defp deps do [ ... ]` list.
    new =
      String.replace(
        content,
        ~r/(defp deps do\s*\[\s*)/,
        "\\1{:local_live_view, path: \"#{llv_path}\"},\n      ",
        global: false
      )

    File.write!(mix_path, new)
  end

  # Mix doesn't materialize path deps under deps/, but Phoenix's esbuild resolves
  # `import "local_live_view"` via NODE_PATH=deps. Symlink it in so the bundle resolves
  # exactly like a real hex/git install (deps/local_live_view/priv/static/local_live_view.js).
  defp link_llv_into_deps(app_dir, llv_path) do
    link = Path.join([app_dir, "deps", "local_live_view"])
    File.rm_rf!(link)
    File.ln_s!(llv_path, link)
  end

  defp patch_router(app_dir) do
    path = Path.join(app_dir, "lib/test_app_web/router.ex")
    content = File.read!(path)

    content =
      String.replace(
        content,
        "get \"/\", PageController, :home",
        "get \"/\", PageController, :home\n\n    live_local \"/hello_local\", \"HelloLocal\""
      )

    File.write!(path, content)
  end

  defp kill_port(port) do
    System.shell("lsof -ti :#{port} | xargs -r kill -9 2>/dev/null", into: "")
  end

  defp kill_phoenix_subprocesses do
    # Watchers don't bind the port — match by command name.
    kill_port(@port)
    System.shell("pkill -f 'esbuild.*test_app' 2>/dev/null", into: "")
    System.shell("pkill -f 'tailwind.*test_app' 2>/dev/null", into: "")
  end

  defp start_phoenix_server(project_dir) do
    Task.start_link(fn ->
      System.shell(
        "mix phx.server",
        cd: project_dir,
        env: [{"PORT", to_string(@port)}, {"MIX_ENV", "dev"}],
        into: IO.stream(:stdio, :line)
      )
    end)
  end

  defp run!(cmd, args, opts) do
    {_, 0} = System.cmd(cmd, args, [{:into, IO.stream(:stdio, :line)} | opts])
  end

  defp wait_for(fun, timeout) when timeout <= 0, do: fun.()

  defp wait_for(fun, timeout) do
    try do
      fun.()
    rescue
      ExUnit.AssertionError ->
        Process.sleep(100)
        wait_for(fun, timeout - 100)
    end
  end
end
