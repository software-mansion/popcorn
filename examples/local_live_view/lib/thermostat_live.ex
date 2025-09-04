defmodule ThermostatLive do
  use LocalLiveView

  def render(assigns) do
    ~H"""
    <p>Current temperature: {@temperature}°F</p>
    <button phx-click="inc_temperature">+</button>
    <p>Country: {@country}</p>
    """
  end

  def mount(_params, _session, socket) do
    temperature = 160
    country = "Greece"
    socket =
      socket
      |> assign(:temperature, temperature)
      |> assign(:country, country)
    {:ok, socket}
  end

#  def handle_event("inc_temperature", _params, socket) do
#    {:noreply, update(socket, :temperature, &(&1 + 1))}
#  end
end
