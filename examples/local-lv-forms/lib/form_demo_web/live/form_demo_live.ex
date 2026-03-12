defmodule FormDemoWeb.FormDemoLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~H"""
    <div class="centered-div">
      <div data-pop-view="FormDemoLocal" phx-hook="ServerSendHook" id="FormDemoLocal"></div>

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
        "llv_local_message",
        %{"payload" => %{"type" => "new_user", "user" => user}},
        socket
      ) do
    new_users = socket.assigns.users ++ [user]
    Application.put_env(FormDemo, :users, new_users)
    socket = push_event(socket, "llv_rerender", %{"view" => "FormDemoLocal"})
    {:noreply, assign(socket, users: new_users)}
  end

  def handle_event("llv_local_message", %{"payload" => %{"type" => "sync_request"}}, socket) do
    users = Application.get_env(FormDemo, :users, [])
    payload = %{"type" => "synchronize", "users" => users}

    socket =
      push_event(socket, "llv_server_message", %{"view" => "FormDemoLocal", "payload" => payload})

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
      push_event(socket, "llv_server_message", %{"view" => "FormDemoLocal", "payload" => payload})

    assign(socket, users: users)
  end
end
