defmodule Echo do
  # Renders what it gets, for checking server-side rendering and attributes.
  use LocalLiveView

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :connected, connected?(socket))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"#{@id}-content"}>
      <p class="label">{@label}</p>
      <p class="connected">connected: {@connected}</p>
      <p class="main-attr">main attr: {assigns[:main]}</p>
      <p class="url-attr">url attr: {assigns[:url]}</p>
    </div>
    """
  end
end
