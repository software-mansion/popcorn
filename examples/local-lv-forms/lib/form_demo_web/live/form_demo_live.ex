defmodule FormDemoWeb.FormDemoLive do
  use FormDemoWeb, :live_view

  def render(assigns) do
    ~H"""
    <div class="centered-div">
      <.local_live_view view="FormDemoLocal" />
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
    send(self(), :sync)
    {:ok, assign(socket, users: [])}
  end

  def handle_event(
        "new_user",
        %{"user" => user},
        socket
      ) do
    new_users = socket.assigns.users ++ [user]
    Application.put_env(FormDemo, :users, new_users)
    {:noreply, assign(socket, users: new_users)}
  end

  def handle_event("sync_request", _, socket) do
    users = Application.get_env(FormDemo, :users, [])
    payload = %{"type" => "synchronize", "users" => users}

    socket =
      push_to_local(socket, "FormDemoLocal", payload)

    socket = assign(socket, users: users)
    {:noreply, socket}
  end

  def handle_info(:sync, socket) do
    {:noreply, sync_local_users(socket)}
  end

  defp sync_local_users(socket) do
    users = Application.get_env(FormDemo, :users, [])
    payload = %{"type" => "synchronize", "users" => users}

    socket =
      push_to_local(socket, "FormDemoLocal", payload)

    assign(socket, users: users)
  end
end
