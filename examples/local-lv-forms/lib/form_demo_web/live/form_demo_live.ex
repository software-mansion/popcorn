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
    Phoenix.PubSub.subscribe(FormDemo.PubSub, "llv_mirror:FormDemoLocal")
    attrs = LocalLiveView.Channel.get_attrs("FormDemoLocal")
    {:ok, assign(socket, users: Map.get(attrs, "users", []))}
  end

  def handle_info({:llv_attrs, attrs}, socket) do
    {:noreply, assign(socket, users: Map.get(attrs, "users", []))}
  end
end
