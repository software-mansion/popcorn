defmodule FormDemoWeb.FormDemoLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~H"""
    <div class="centered-div">
      <div data-pop-view="FormDemoLocal"></div>
    </div>
    <div class="bordered">
      <h1>[Server] User List:</h1>
      <ul>
        <%= for user <- @users do %>
          <li>Username: <%= user["username"] %>, Email: <%= user["email"] %></li>
        <% end %>
      </ul>
    </div>
    """
  end

  def mount(_params, _session, socket) do
    users = []
    socket = push_event(socket, "llv_rerender", %{"view" => "FormDemoLocal"})
    {:ok, assign(socket, users: users)}
  end

  def handle_event("llv_message", %{"view" => view, "payload" => payload}, socket) do
    IO.inspect("LLV MESSAGE RECEIVED\nVIEW: #{inspect(view)} PAYLOAD: #{inspect(payload)}")
    {:noreply, socket}
  end

end
