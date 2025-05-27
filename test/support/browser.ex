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
    Agent.start_link(fn -> %{browser: browser, url: url} end, name: __MODULE__)

    :ok
  end

  def new_page(bundle_path, log_handler \\ fn _log -> :ok end) do
    %{browser: browser, url: url} = Agent.get(__MODULE__, & &1)
    url = "#{url}?bundlePath=#{Path.relative_to_cwd(bundle_path)}"
    page = Playwright.Browser.new_page(browser)

    on_exit(fn ->
      # in debug mode we want to keep the page open to allow interaction with it
      unless debug_mode?(), do: Playwright.Page.close(page)
    end)

    response = Playwright.Page.goto(page, url, %{wait_until: :domcontentloaded})
    assert response.status == 200

    # wait until sure Popcorn is initialized
    Playwright.Page.evaluate(page, """
    async () => {window.popcorn = await window.popcorn_promise;}
    """)

    # capture unhandled errors form the iframe window and re-log them again
    # otherwise Playwright won't capture them
    Playwright.Page.evaluate(page, """
    const popcorn_iframe = document.querySelector("iframe")
    popcorn_iframe.contentWindow.addEventListener("error", (event) => {
    const message = event.error ? event.error.message : event.message;
      console.log(`[${event.filename}:${event.lineno}] ${message}`, event)
    })
    """)

    Playwright.Page.on(page, "console", fn %{params: log} ->
      prefix =
        cond do
          log.location.url == "" -> ""
          String.starts_with?(log.text, "[popcorn stdout]") -> ""
          true -> "[#{log.location.url}:#{log.location.lineNumber}] "
        end

      log_handler.("#{prefix}#{log.text}\n")
    end)

    page
  end

  def debug_mode?() do
    System.get_env("DEBUG") == "true"
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
