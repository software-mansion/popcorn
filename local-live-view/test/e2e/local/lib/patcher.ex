defmodule Patcher do
  # A view that isn't the page's main view, patching the page URL.
  use LocalLiveView

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :params_called, false)}
  end

  # Only the main view gets handle_params/3, so this must never run.
  @impl true
  def handle_params(_params, _uri, socket) do
    {:noreply, assign(socket, :params_called, true)}
  end

  @impl true
  def handle_event("patch", _params, socket) do
    {:noreply, push_patch(socket, to: socket.assigns.to)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <button id={"#{@id}-patch"} phx-click="patch">patch to {@to}</button>
      <p id={"#{@id}-params-called"}>params called: {@params_called}</p>
    </div>
    """
  end
end
