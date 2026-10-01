defmodule SlowLocal do
  # A hosted view whose mount takes a while in the browser, so what's on the
  # page before then comes from the server.
  use LocalLiveView

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket), do: Process.sleep(3000)
    {:ok, assign(socket, :connected, connected?(socket))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <p id="slow-local">slow content, connected: {@connected}</p>
    """
  end
end
