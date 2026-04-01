defmodule FormDemoWeb.FormDemoLive do
  use FormDemoWeb, :live_view
  use LocalLiveView.Phoenix

  def render(assigns) do
    ~H"""
    <div class="centered-div">
      <.local_live_view view="FormDemoLocal" attrs={%{users: @users}} />
      <div class="bordered">
        <h1>[Server Runtime] User List:</h1>
        <ul>
          <%= for user <- @users do %>
            <li>Username: {user["username"]}, Email: {user["email"]}</li>
          <% end %>
        </ul>
      </div>
    </div>
    """
  end

  def mount(_params, _session, socket) do
    {:ok, assign(socket, users: [])}
  end

  @impl LocalLiveView.Phoenix
  def handle_local_sync(%{"users" => new_users}, socket) do
    {:noreply, assign(socket, users: new_users)}
  end
end
