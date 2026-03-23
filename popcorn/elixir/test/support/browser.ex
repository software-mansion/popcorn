defmodule Popcorn.Support.Browser do
  @moduledoc false

  import ExUnit.Assertions
  import ExUnit.Callbacks

  def launch(options \\ []) do
    port = Keyword.get(options, :port, 9876)
    url = "http://localhost:#{port}"

    Task.start_link(fn -> System.shell("elixir test/fixtures/wasm/server.exs --port #{port}") end)

    # Wait until the server is ready
    {_pid, tmp_browser} = Playwright.BrowserType.launch(:chromium)
    page = Playwright.Browser.new_page(tmp_browser)
    wait_for(fn -> assert %{status: 200} = Playwright.Page.goto(page, url) end, 60_000)
    Playwright.Browser.close(tmp_browser)

    if debug_mode?() do
      ExUnit.after_suite(fn _result -> IO.getn("Press enter to exit\n") end)
      Application.put_env(:playwright, LaunchOptions, devtools: true)
    end

    {_pid, browser} = Playwright.BrowserType.launch(:chromium)

    # Create a single shared page for all tests — each test runs
    # in its own iframe (Popcorn instance) on this page.
    shared_page = Playwright.Browser.new_page(browser)
    response = Playwright.Page.goto(shared_page, url, %{wait_until: :domcontentloaded})
    assert response.status == 200

    wait_for(fn ->
      result = Playwright.Page.evaluate(shared_page, "() => typeof window.createPopcornInstance")
      assert result == "function"
    end)

    Agent.start_link(fn -> %{browser: browser, page: shared_page} end, name: __MODULE__)

    ExUnit.after_suite(fn _result ->
      try do
        Playwright.Browser.close(browser)
      rescue
        _ -> :ok
      end
    end)

    :ok
  end

  @doc """
  Creates a new Popcorn instance (iframe) on the shared page.
  Returns {page, instance_id} for use with evaluate calls.
  The instance is destroyed automatically after the test via on_exit.
  """
  def new_instance(bundle_path) do
    %{page: page} = Agent.get(__MODULE__, & &1)
    relative_path = Path.relative_to_cwd(bundle_path)

    instance_id =
      Playwright.Page.evaluate(page, """
      async () => await window.createPopcornInstance("#{relative_path}")
      """)

    on_exit(fn ->
      if not debug_mode?() do
        try do
          Playwright.Page.evaluate(page, """
          () => window.destroyPopcornInstance("#{instance_id}")
          """)
        rescue
          _ -> :ok
        end
      end
    end)

    {page, instance_id}
  end

  def debug_mode?() do
    System.get_env("DEBUG") == "true"
  end

  def wait_for(fun, timeout \\ 5_000)

  def wait_for(fun, timeout) when timeout <= 0 do
    fun.()
  end

  def wait_for(fun, timeout) do
    try do
      fun.()
    rescue
      ExUnit.AssertionError ->
        dt = 100
        Process.sleep(dt)
        wait_for(fun, timeout - dt)
    end
  end
end
