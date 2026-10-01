defmodule SlowMain do
  # A main view whose mount takes a while in the browser, so the URL can change
  # while it's still mounting.
  use LocalLiveView

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket), do: Process.sleep(3000)
    {:ok, assign(socket, tab: nil)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    {:noreply, assign(socket, :tab, params["tab"] || "none")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <p id="slow-main-tab">tab: {@tab}</p>
    """
  end
end
