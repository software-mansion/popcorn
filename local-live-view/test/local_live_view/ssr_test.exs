defmodule LocalLiveView.SSRTest do
  use ExUnit.Case, async: true

  alias LocalLiveView.SSR

  defmodule Greeter do
    use LocalLiveView

    @impl true
    def mount(params, session, socket) do
      send(self(), {:mounted, params, session, connected?(socket)})
      {:ok, assign(socket, greeting: "Hello", tab: "none")}
    end

    @impl true
    def update(%{name: name}, socket) do
      {:ok, assign(socket, :name, name)}
    end

    @impl true
    def handle_params(params, url, socket) do
      send(self(), {:params, params, url})
      {:noreply, assign(socket, :tab, params["tab"] || "none")}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <p>{@greeting}, {@name}!</p>
      <.badge label={@tab} />
      <.live_component module={LocalLiveView.SSRTest.Counter} id="counter" start={3} />
      """
    end

    defp badge(assigns) do
      ~H"""
      <span class="badge">{@label}</span>
      """
    end
  end

  defmodule Counter do
    use Phoenix.LiveComponent

    @impl true
    def update(%{start: start}, socket) do
      {:ok, assign(socket, count: start * 2)}
    end

    @impl true
    def render(assigns) do
      ~H"<b>{@count}</b>"
    end
  end

  defmodule Minimal do
    use LocalLiveView

    @impl true
    def render(assigns) do
      ~H"<i>{inspect(Map.keys(@__changed__))}</i>"
    end
  end

  defmodule Redirecting do
    use LocalLiveView

    @impl true
    def mount(_params, _session, socket) do
      {:ok, redirect(socket, to: "/elsewhere")}
    end

    @impl true
    def render(assigns), do: ~H"never"
  end

  defmodule Broken do
    use LocalLiveView

    @impl true
    def mount(_params, _session, _socket), do: :oops

    @impl true
    def render(assigns), do: ~H"never"
  end

  defp html(view, assigns, opts \\ []) do
    assert {:ok, iodata} = SSR.render(view, assigns, opts)
    IO.iodata_to_binary(iodata)
  end

  test "runs mount, update, handle_params and render, including components" do
    html = html(inspect(Greeter), %{name: "world"}, url: "http://localhost:4000/page?tab=news")

    assert html =~ "<p>Hello, world!</p>"
    assert html =~ ~s(<span class="badge">news</span>)
    assert html =~ "<b>6</b>"
  end

  test "the view is not connected and is not mounted at the router" do
    html(inspect(Greeter), %{name: "world"})
    assert_received {:mounted, :not_mounted_at_router, %{}, false}
  end

  test "handle_params gets no params and a nil url when the url is unknown" do
    html = html(inspect(Greeter), %{name: "world"})
    assert_received {:params, %{}, nil}
    assert html =~ ~s(<span class="badge">none</span>)
  end

  test "works for a view defining only render/1" do
    assert html(inspect(Minimal), %{}) == "<i>[]</i>"
  end

  test "returns an error for an unknown view" do
    assert {:error, :not_loaded} = SSR.render("Nope.Missing", %{})
  end

  test "returns an error when the view redirects in mount" do
    assert {:error, :redirected} = SSR.render(inspect(Redirecting), %{})
  end

  test "propagates errors raised by the view" do
    assert_raise ArgumentError, ~r/invalid return from .*Broken.mount\/3/, fn ->
      SSR.render(inspect(Broken), %{})
    end
  end
end
