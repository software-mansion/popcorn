defmodule Hosted do
  # A view rendered inside a host LiveView (LocalLiveView.E2E.HostLive), which
  # passes it `count`, `rev`, `tab`, `path` and `nav_to`.
  use LocalLiveView

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       local_clicks: 0,
       form_value: "",
       submitted: "",
       drag_events: [],
       mouse_event: nil
     )}
  end

  # Optimistic: bumped here, then confirmed or rolled back by the host.
  @impl true
  def handle_event("inc", _params, socket) do
    {:noreply,
     socket
     |> update(:count, &(&1 + 1))
     |> push_server_event("inc", %{})}
  end

  # The host rejects it and re-sends its assigns.
  def handle_event("reject", _params, socket) do
    {:noreply,
     socket
     |> update(:count, &(&1 + 100))
     |> push_server_event("reject", %{})}
  end

  def handle_event("local_inc", _params, socket) do
    {:noreply, update(socket, :local_clicks, &(&1 + 1))}
  end

  def handle_event("patch", _params, socket) do
    {:noreply, push_patch(socket, to: "#{socket.assigns.path}?tab=hosted-patch")}
  end

  def handle_event("navigate", _params, socket) do
    {:noreply, push_navigate(socket, to: socket.assigns.nav_to)}
  end

  # push_navigate from handle_info reaches the browser as a push, not as the
  # reply to an event.
  def handle_event("navigate_later", _params, socket) do
    send(self(), :navigate)
    {:noreply, socket}
  end

  def handle_event("change", %{"value" => value}, socket) do
    {:noreply, assign(socket, :form_value, value)}
  end

  def handle_event("submit", %{"value" => value}, socket) do
    {:noreply, assign(socket, :submitted, value)}
  end

  def handle_event(drag, params, socket) when drag in ~w(drag_start drag_over drag_end) do
    event = "#{drag}:#{pointer_data?(params)}"
    {:noreply, update(socket, :drag_events, &(&1 ++ [event]))}
  end

  def handle_event("mouse_down", params, socket) do
    {:noreply, assign(socket, :mouse_event, "mouse_down:#{pointer_data?(params)}")}
  end

  @impl true
  def handle_info(:navigate, socket) do
    {:noreply, push_navigate(socket, to: socket.assigns.nav_to)}
  end

  defp pointer_data?(params), do: is_map(params["rect"]) and is_number(params["clientX"])

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <p id="hosted-count">count: {@count}</p>
      <p id="hosted-tab">tab: {@tab}</p>
      <button id="hosted-inc" phx-click="inc">inc</button>
      <button id="hosted-reject" phx-click="reject">reject</button>
      <button id="hosted-local-inc" phx-click="local_inc">local clicks: {@local_clicks}</button>
      <button id="hosted-patch" phx-click="patch">push_patch</button>
      <button id="hosted-navigate" phx-click="navigate">push_navigate to {@nav_to}</button>
      <button id="hosted-navigate-later" phx-click="navigate_later">push_navigate later</button>
      <form id="hosted-form" phx-change="change" phx-submit="submit">
        <input id="hosted-input" name="value" value={@form_value} />
        <button id="hosted-submit" type="submit">submit</button>
      </form>
      <p id="hosted-form-value">value: {@form_value}</p>
      <p id="hosted-submitted">submitted: {@submitted}</p>
      <div
        id="hosted-draggable"
        draggable="true"
        phx-dragstart="drag_start"
        phx-dragover="drag_over"
        phx-dragend="drag_end"
      >
        drag me
      </div>
      <p id="hosted-drag-events">{Enum.join(@drag_events, ",")}</p>
      <div id="hosted-mouse" phx-mousedown="mouse_down">mouse</div>
      <p id="hosted-mouse-event">{@mouse_event}</p>
    </div>
    """
  end
end
