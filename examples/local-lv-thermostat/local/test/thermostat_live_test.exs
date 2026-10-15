defmodule ThermostatLiveTest do
  @moduledoc """
  Runs the offline (LocalLiveView) thermostat: mounts the view, handles the
  increment/decrement events and renders each state to HTML. Popcorn runs the
  same BEAM in the browser, so the view behaves there as it does here.
  """
  use ExUnit.Case

  test "ThermostatLive mount/handle_event/render" do
    {:ok, socket} = ThermostatLive.mount(%{}, %{}, %Phoenix.LiveView.Socket{})
    {:noreply, hotter} = ThermostatLive.handle_event("inc_temperature", %{}, socket)
    {:noreply, colder} = ThermostatLive.handle_event("dec_temperature", %{}, socket)

    assert render(socket) =~ "Current temperature: 25°C"
    assert render(socket) =~ "Country: Poland"
    assert render(hotter) =~ "Current temperature: 26°C"
    assert render(colder) =~ "Current temperature: 24°C"
  end

  defp render(socket) do
    socket.assigns
    |> Map.put(:__changed__, nil)
    |> ThermostatLive.render()
    |> Phoenix.HTML.Safe.to_iodata()
    |> IO.iodata_to_binary()
  end
end
