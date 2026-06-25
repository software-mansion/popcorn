defmodule NavTestLive do
  use LocalLiveView

  def mount(_params, _session, socket) do
    {:ok, assign(socket, page: "products", count: 0)}
  end

  def handle_params(%{"page" => page}, _uri, socket) when page in ["products", "orders", "profile"] do
    {:noreply, assign(socket, page: page)}
  end

  def handle_params(_params, _uri, socket) do
    {:noreply, assign(socket, page: "products")}
  end

  def handle_event("inc", _params, socket) do
    {:noreply, assign(socket, count: socket.assigns.count + 1)}
  end

  def render(assigns) do
    ~H"""
    <div style="padding: 16px;">
      <div style="display: flex; gap: 4px; margin-bottom: 20px; border-bottom: 2px solid #e5e7eb; padding-bottom: 0;">
        <.tab_link page={@page} target="products" label="Products" />
        <.tab_link page={@page} target="orders" label="Orders" />
        <.tab_link page={@page} target="profile" label="Profile" />
      </div>

      <%= case @page do %>
        <% "products" -> %>
          <div>
            <h3 style="font-weight: 600; color: #111827; margin-bottom: 12px;">Products</h3>
            <p style="color: #6b7280; font-size: 14px;">Counter (persists across tab navigation): <%= @count %></p>
            <button
              phx-click="inc"
              style="margin-top: 8px; padding: 6px 14px; background-color: #3b82f6; color: white; border-radius: 4px; border: none; cursor: pointer; font-size: 14px;"
            >
              + Increment
            </button>
          </div>
        <% "orders" -> %>
          <div>
            <h3 style="font-weight: 600; color: #111827; margin-bottom: 12px;">Orders</h3>
            <p style="color: #6b7280; font-size: 14px;">No orders yet.</p>
          </div>
        <% "profile" -> %>
          <div>
            <h3 style="font-weight: 600; color: #111827; margin-bottom: 12px;">Profile</h3>
            <p style="color: #6b7280; font-size: 14px;">User profile settings.</p>
          </div>
      <% end %>
    </div>
    """
  end

  defp tab_link(assigns) do
    assigns = assign(assigns, :active, assigns.page == assigns.target)
    ~H"""
    <.link
      patch={"/plain/?page=#{@target}"}
      style={"padding: 8px 16px; font-size: 14px; font-weight: 500; border-bottom: 2px solid #{if @active, do: "#3b82f6", else: "transparent"}; color: #{if @active, do: "#3b82f6", else: "#6b7280"}; text-decoration: none; margin-bottom: -2px;"}
    >
      {@label}
    </.link>
    """
  end
end
