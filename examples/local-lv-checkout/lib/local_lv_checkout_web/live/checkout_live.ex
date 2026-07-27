defmodule LocalLvCheckoutWeb.CheckoutLive do
  use LocalLvCheckoutWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       llv_id: "checkout-#{socket.id}",
       nav_llv_id: "nav-#{socket.id}",
       current_step: 1
     )}
  end

  def handle_params(%{"step" => step}, _uri, socket) do
    {:noreply, assign(socket, current_step: String.to_integer(step))}
  catch
    _ -> {:noreply, socket}
  end

  def handle_params(_params, _uri, socket) do
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div style="min-height: 100vh; background-color: #f3f4f6; padding: 48px 16px;">
      <div style="max-width: 42rem; margin: 0 auto;">
        <!-- Step Indicator -->
        <div style="margin-bottom: 32px; background-color: white; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); padding: 24px;">
          <div style="display: flex; align-items: center; justify-content: space-between;">
            <div style="display: flex; flex-direction: column; align-items: center; flex: 1;">
              <div style={"width: 40px; height: 40px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; transition: all 0.3s; background-color: #{if @current_step >= 1, do: "#3b82f6", else: "#d1d5db"}; color: #{if @current_step >= 1, do: "white", else: "#4b5563"};"}>
                1
              </div>
              <p style="font-size: 14px; margin-top: 8px; color: #6b7280;">Shipping</p>
            </div>
            <div style={"height: 4px; flex: 1; margin: 0 16px; background-color: #{if @current_step > 1, do: "#3b82f6", else: "#d1d5db"};"}>
            </div>
            <div style="display: flex; flex-direction: column; align-items: center; flex: 1;">
              <div style={"width: 40px; height: 40px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; transition: all 0.3s; background-color: #{if @current_step >= 2, do: "#3b82f6", else: "#d1d5db"}; color: #{if @current_step >= 2, do: "white", else: "#4b5563"};"}>
                2
              </div>
              <p style="font-size: 14px; margin-top: 8px; color: #6b7280;">Payment</p>
            </div>
            <div style={"height: 4px; flex: 1; margin: 0 16px; background-color: #{if @current_step > 2, do: "#3b82f6", else: "#d1d5db"};"}>
            </div>
            <div style="display: flex; flex-direction: column; align-items: center; flex: 1;">
              <div style={"width: 40px; height: 40px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; transition: all 0.3s; background-color: #{if @current_step >= 3, do: "#3b82f6", else: "#d1d5db"}; color: #{if @current_step >= 3, do: "white", else: "#4b5563"};"}>
                3
              </div>
              <p style="font-size: 14px; margin-top: 8px; color: #6b7280;">Confirm</p>
            </div>
          </div>
        </div>

        <!-- Local Live View -->
        <div style="background-color: white; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);">
          <.local_live_view id={"checkout-#{@socket.id}"} view="CheckoutLive" />
        </div>
      </div>
    </div>
    """
  end
end
