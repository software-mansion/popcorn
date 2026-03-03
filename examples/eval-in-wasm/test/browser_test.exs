defmodule EvalInWasm.BrowserTest do
  use ExUnit.Case

  setup_all do
    {:ok, _apps} = Application.ensure_all_started(:playwright)
    {_pid, browser} = Playwright.BrowserType.launch(:chromium)

    on_exit(fn ->
      Playwright.Browser.close(browser)
    end)

    port = 9876
    url = "http://localhost:#{port}"

    Task.start_link(fn -> System.shell("elixir server.exs --port #{port}") end)

    # Wait until the server is ready
    page = Playwright.Browser.new_page(browser)
    wait_for(fn -> assert %{status: 200} = Playwright.Page.goto(page, url) end, 60_000)
    Playwright.Page.close(page)

    [browser: browser, url: url]
  end

  setup %{browser: browser, url: url} do
    page = Playwright.Browser.new_page(browser)
    response = Playwright.Page.goto(page, url)
    assert response.status == 200
    # Wait for JS to be ready
    Process.sleep(1_000)

    on_exit(fn ->
      Playwright.Page.close(page)
    end)

    [page: page]
  end

  test "case", %{page: page} do
    run_example(page, "case", "{:ok, 3}")
  end

  @tag :skip
  test "module", %{page: page} do
    run_example(page, "module", "{:sum, 30}")
  end

  defp run_example(page, name, result) do
    example_button = Playwright.Page.get_by_text(page, "Example: #{name}")
    Playwright.Locator.click(example_button)
    Playwright.Page.click(page, ~s|button[id="eval"]|)
    wait_for(fn -> assert Playwright.Page.text_content(page, ~s|[id="result"]|) == result end)
  end

  defp wait_for(fun, timeout \\ 5_000)

  defp wait_for(fun, timeout) when timeout <= 0 do
    fun.()
  end

  defp wait_for(fun, timeout) do
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
