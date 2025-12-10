defmodule FormDemoWeb.FormDemoLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~H"""
    <div class="centered-div">
      <div data-pop-view="FormDemoLocal" phx-hook="ServerSendHook" id="FormDemoLocal"></div>
      
      <div class="bordered">
        <h1>[Server] User List:</h1>
        <ul>
          <%= for user <- @users do %>
            <li>Username: <%= user["username"] %>, Email: <%= user["email"] %></li>
          <% end %>
        </ul>
        <div class="centered">
          <button class="ghost-button" phx-click="synchronize">SYNCHRONIZE</button>
        </div>
      </div>
    </div>
    """
  end

  def mount(_params, session, socket) do
    users = Application.get_env(FormDemo, :users, [])
    socket = push_event(socket, "llv_rerender", %{"view" => "FormDemoLocal"})
    {:ok, assign(socket, users: users)}
  end

  def handle_event("llv_local_message", %{"view" => view, "payload" => payload}, socket) do
    IO.inspect("LLV MESSAGE RECEIVED\nVIEW: #{inspect(view)} PAYLOAD: #{inspect(payload)}")
    IO.inspect(socket.private, limit: :infinity)
    new_users = socket.assigns.users ++ [payload]
    users = Application.put_env(FormDemo, :users, new_users)
    {:noreply, assign(socket, users: new_users)}
  end
  
  def handle_event("synchronize", _params, socket) do
    users = Application.get_env(FormDemo, :users, [])
    payload = %{"users" => users}
    socket = push_event(socket, "llv_server_message", %{"view" => "FormDemoLocal", "payload" => payload})
    {:noreply, socket}
  end

end
