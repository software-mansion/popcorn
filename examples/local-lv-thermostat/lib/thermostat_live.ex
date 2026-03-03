defmodule ThermostatLive do
  use LocalLiveView

  def render(assigns) do
    ~H"""
    <p>Current temperature: {@temperature}°C</p>
    <div class="thermostat-controls">
      <button pop-click="inc_temperature" class="ghost-button">+</button>
      <button pop-click="dec_temperature" class="ghost-button">-</button>
    </div>
    <p>Country: {@country}</p>
    """
  end

  def mount(_params, _session, socket) do
    temperature = 25
    country = "Poland"

    socket =
      socket
      |> assign(:temperature, temperature)
      |> assign(:country, country)

    {:ok, socket}
  end

  def handle_event("inc_temperature", _params, socket) do
    {:noreply, update(socket, :temperature, &(&1 + 1))}
  end

  def handle_event("dec_temperature", _params, socket) do
    {:noreply, update(socket, :temperature, &(&1 - 1))}
  end
end
