defmodule ThermostatLivePresentation do
  use LocalLiveView

  defdelegate render(assigns), to: ThermostatLive

  def mount(params, session, socket) do
    result = ThermostatLive.mount(params, session, socket)
    {:ok, new_socket} = result

    Popcorn.Wasm.send_event("llv_presentation", %{
      block: nil,
      event: "mount",
      assigns: presentation_assigns(new_socket)
    })

    result
  end

  def handle_event(event, params, socket)
      when event in ["inc_temperature", "dec_temperature"] do

    effective_socket =
      case socket.assigns[:_pending_cur] do
        nil -> socket
        pending -> assign(socket, Map.to_list(pending))
      end

    {:noreply, updated} = ThermostatLive.handle_event(event, params, effective_socket)

    Popcorn.Wasm.send_event("llv_presentation", %{
      block: event,
      event: event,
      assigns: presentation_assigns(updated)
    })

    new_pending = %{
      temperature: updated.assigns.temperature,
      country: updated.assigns.country
    }

    {:noreply,
     socket
     |> assign(:_pending_prev, socket.assigns[:_pending_cur])
     |> assign(:_pending_cur, new_pending)}
  end

  def handle_info({:js_push, "llv_ack", _}, socket) do
    case {socket.assigns[:_pending_prev], socket.assigns[:_pending_cur]} do
      {%{temperature: t, country: c}, _} ->
        {:noreply,
         socket
         |> assign(:temperature, t)
         |> assign(:country, c)
         |> assign(:_pending_prev, nil)}

      {nil, %{temperature: t, country: c}} ->
        {:noreply,
         socket
         |> assign(:temperature, t)
         |> assign(:country, c)
         |> assign(:_pending_cur, nil)}

      _ ->
        {:noreply, socket}
    end
  end

  def handle_event(event, params, socket) do
    ThermostatLive.handle_event(event, params, socket)
  end

  defp presentation_assigns(socket) do
    %{
      temperature: socket.assigns.temperature,
      country: socket.assigns.country
    }
  end
end
