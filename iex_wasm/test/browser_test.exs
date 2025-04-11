defmodule IexWasm.BrowserTest do
  use ExUnit.Case

  setup_all do
    {_pid, browser} = Playwright.BrowserType.launch(:chromium)

    on_exit(fn ->
      Playwright.Browser.close(browser)
    end)

    port = 9876

    System.argv(System.argv() ++ ["--port", "#{port}", "--no-wait"])
    Code.eval_file("server.exs")

    [browser: browser, url: "http://localhost:#{port}"]
  end

  setup %{browser: browser, url: url} do
    page = Playwright.Browser.new_page(browser)
    response = Playwright.Page.goto(page, url)

    assert response.status == 200

    on_exit(fn ->
      Playwright.Page.close(page)
    end)

    [page: page]
  end

  test "case", %{page: page} do
    Playwright.Page.click(page, ~s|button[id="example-case"]|)
    Playwright.Page.click(page, ~s|button[id="eval"]|)
    assert_result(page, "{:ok, 3}")
  end

  test "module", %{page: page} do
    Playwright.Page.click(page, ~s|button[id="example-module"]|)
    Playwright.Page.click(page, ~s|button[id="eval"]|)
    assert_result(page, "{:sum, 30}")
  end

  defp assert_result(page, expected, timeout \\ 5000) do
    result = Playwright.Page.text_content(page, "[id=\"result\"]")

    cond do
      timeout <= 0 ->
        assert result == expected

      result == expected ->
        result

      true ->
        dt = 100
        Process.sleep(dt)
        assert_result(page, expected, timeout - dt)
    end
  end
end
