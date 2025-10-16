defmodule CompareLiveViewsWeb.DemoModalOnline do
  use CompareLiveViewsWeb, :live_view

  def render(assigns) do
    ~H"""
    <p>PHOENIX LIVE VIEW</p>

    <%= if @show_modal do %>
      <div class="modal">
        <div class="modal-content">
          <span class="close" phx-click="close_modal">&times;</span>
          <p>ONLINE MODAL</p>
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
end
