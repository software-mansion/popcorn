defmodule FormDemoWeb.FormDemoLive do
  use FormDemoWeb, :live_view

  def render(assigns) do
    ~H"""
    <div class="centered-div">
      <.local_live_view id={"form-demo-local-#{@socket.id}"} view="FormDemoLocal" />
      <div class="bordered">
        <h2>[Server Runtime] User List:</h2>
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
    if connected?(socket) do
      llv_id = "form-demo-local-#{socket.id}"
      Phoenix.PubSub.subscribe(FormDemo.PubSub, "llv_mirror:FormDemoLocal:#{llv_id}")
      users = LocalLiveView.Channel.get_mirror_assigns(llv_id) |> Map.get("users", [])
      {:ok, assign(socket, users: users)}
    else
      {:ok, assign(socket, users: [])}
    end
  end

  def handle_info({:llv_attrs, attrs}, socket) do
    {:noreply, assign(socket, users: Map.get(attrs, "users", []))}
  end
end
