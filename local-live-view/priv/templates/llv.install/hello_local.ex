defmodule HelloLocal do
  use LocalLiveView

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <p>Hello from WASM!</p>
    </div>
    """
  end

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end
end
