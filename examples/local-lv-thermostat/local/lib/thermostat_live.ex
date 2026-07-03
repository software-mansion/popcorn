defmodule ThermostatLive do
  use LocalLiveView

  @default_temperature 25
  @default_country "Poland"

  def render(assigns) do
    ~H"""
    <div class="thermostat-wrapper">
      <p class="thermostat-temp-label">Current temperature</p>
      <p class="thermostat-temp-value" data-value="temperature">{@temperature}°C</p>
      <div class="thermostat-controls">
        <button phx-click="inc_temperature" class="ghost-button">+</button>
        <button phx-click="dec_temperature" class="ghost-button">-</button>
      </div>
      <p class="thermostat-country">Country: {@country}</p>
    </div>
    """
  end

  def mount(_params, _session, socket) do
    {:ok, socket |> assign(:temperature, @default_temperature) |> assign(:country, @default_country)}
  end

  def handle_event("inc_temperature", _params, socket) do
    {:noreply, update(socket, :temperature, &(&1 + 1))}
  end

  def handle_event("dec_temperature", _params, socket) do
    {:noreply, update(socket, :temperature, &(&1 - 1))}
  end

end
