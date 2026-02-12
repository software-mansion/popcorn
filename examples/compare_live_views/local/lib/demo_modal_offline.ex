defmodule DemoModalOffline do
  use LocalLiveView

  def render(assigns) do
    ~H"""
    <p>LOCAL LIVE VIEW</p>
    <button class="show-modal-button" phx-click="show_modal">SHOW OFFLINE MODAL</button>

    <%= if @show_modal do %>
    <div class="modal">
      <div class="modal-content">
        <span class="close" phx-click="close_modal">&times;</span>
        <p>OFFLINE MODAL</p>
      </div>
    </div>
    <% end %>
    """
  end

  def mount(_params, _session, socket) do
    {:ok, Phoenix.LiveView.Utils.assign(socket, :show_modal, true)}
  end

  def handle_event("close_modal", _params, socket) do
    {:noreply, assign(socket, show_modal: false)}
  end
  
  def handle_event("show_modal", _params, socket) do
    {:noreply, assign(socket, show_modal: true)}
  end
end
