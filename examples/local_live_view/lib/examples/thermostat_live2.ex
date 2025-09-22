defmodule ThermostatLive2 do
  use LocalLiveView

  def render(assigns) do
    ~H"""
    <p>Current temperature: {@temperature}°F</p>
    <button pop-click="inc_temperature">+</button>
    <button pop-click="dec_temperature">-</button>
    <p>Country: {@country}</p>
    """
  end

  def mount(_params, _session, socket) do
    temperature = 70
    country = "France"
    socket =
      socket
      |> assign(:temperature, temperature)
      |> assign(:country, country)
    {:ok, socket}
  end

  def handle_event("inc_temperature", _params, socket) do
    temperature = socket.assigns.temperature
    socket = Phoenix.LiveView.Utils.assign(socket, :temperature, temperature+1)
    #    {:noreply, update(socket, :temperature, &(&1 + 1))}
    {:noreply, socket}
  end

  def handle_event("dec_temperature", _params, socket) do
    temperature = socket.assigns.temperature
    socket = Phoenix.LiveView.Utils.assign(socket, :temperature, temperature-1)
    #    {:noreply, update(socket, :temperature, &(&1 + 1))}
    {:noreply, socket}
  end
end
