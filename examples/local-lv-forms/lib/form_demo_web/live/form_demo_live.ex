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
    {:ok, assign(socket, users: Application.get_env(:form_demo, :users, []))}
  end

  @impl LocalLiveView.Phoenix
  def handle_local_sync(%{"users" => new_users}, socket) do
    Application.put_env(:form_demo, :users, new_users)
    {:noreply, assign(socket, users: new_users)}
  end
end
