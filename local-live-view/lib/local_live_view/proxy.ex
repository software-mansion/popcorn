defmodule LocalLiveView.Proxy do
  @moduledoc false
  # A LiveView implementation that wraps each Local LiveView.

  use Phoenix.LiveView

  alias LocalLiveView.Lifecycle
  alias Phoenix.LiveView.Socket

  @impl true
  def mount(params, session, socket) do
    llv = session["llv"]
    view = Lifecycle.resolve_view_module!(llv.view)

    socket =
      socket
      |> put_private(:llv_view, view)
      |> put_private(:llv_id, llv.id)
      |> put_private(:mirror_id, llv.mirror_id)
      |> render_with(&view.render/1)

    {socket, opts} = Lifecycle.mount(view, params, session, socket)

    assigns = decode_assigns(LocalLiveView.Dispatcher.current_assigns(llv.id))
    url = LocalLiveView.Dispatcher.current_url()

    socket =
      socket
      |> then(&Lifecycle.update!(view, assigns, &1))
      |> put_server_assigns(assigns)
      |> then(&Lifecycle.handle_params(view, url, &1))

    LocalLiveView.Dispatcher.register_channel(llv.id, llv.epoch)

    case opts do
      [] -> {:ok, socket}
      opts -> {:ok, socket, opts}
    end
  end

  @impl true
  def handle_event(event, params, socket) do
    view(socket).handle_event(event, params, socket)
  end

  @impl true
  def handle_info({:llv, %{"action" => "update_assigns"} = msg}, socket) do
    %{"assigns" => encoded_assigns} = msg
    assigns = decode_assigns(encoded_assigns)
    socket = Lifecycle.update!(view(socket), assigns, socket)
    {:noreply, put_server_assigns(socket, assigns)}
  end

  def handle_info({:llv, %{"action" => "server_event"} = msg}, socket) do
    %{"params" => params} = msg
    %{"type" => type} = params

    case view(socket).handle_server_event(type, params, socket) do
      {:noreply, %Socket{} = socket} -> {:noreply, socket}
      {:reply, _map, %Socket{} = socket} -> {:noreply, socket}
    end
  end

  def handle_info({:llv, %{"action" => "push_event"} = msg}, socket) do
    %{"event" => event, "params" => params} = msg

    case view(socket).handle_event(event, params, socket) do
      {:noreply, %Socket{} = socket} ->
        {:noreply, socket}

      {:reply, _map, %Socket{} = socket} ->
        {:noreply, socket}

      other ->
        raise ArgumentError, """
        invalid return from #{inspect(view(socket))}.handle_event/3 callback.

        Expected {:noreply, socket} or {:reply, map, socket}, got: #{inspect(other)}
        """
    end
  end

  def handle_info({:llv, %{"action" => "push_error"} = msg}, socket) do
    %{"event" => event, "params" => params} = msg
    server_assigns = socket.private[:llv_server_assigns] || %{}

    case view(socket).handle_push_error(event, params, server_assigns, socket) do
      {:noreply, %Socket{} = socket} ->
        {:noreply, socket}

      other ->
        raise ArgumentError, """
        invalid return from #{inspect(view(socket))}.handle_push_error/4 callback.

        Expected {:noreply, socket}, got: #{inspect(other)}
        """
    end
  end

  def handle_info({:llv, %{"action" => "mirror_reconnected"}}, socket) do
    LocalLiveView.mirror_sync(socket, Map.keys(socket.assigns))
    {:noreply, socket}
  end

  def handle_info({:llv, %{"action" => "handle_params"} = msg}, socket) do
    %{"url" => url} = msg
    {:noreply, Lifecycle.handle_params(view(socket), url, socket)}
  end

  def handle_info({:llv, :patch, to, kind}, socket) do
    push_url_update(to, kind == :replace)
    {:noreply, Lifecycle.handle_params(view(socket), to, socket)}
  end

  def handle_info(msg, socket) do
    view(socket).handle_info(msg, socket)
  end

  ## Proxying helpers

  defp view(%Socket{private: %{llv_view: view}}), do: view

  # Last assigns received from the host, for handle_push_error.
  defp put_server_assigns(socket, assigns) do
    put_private(socket, :llv_server_assigns, assigns)
  end

  defp decode_assigns(nil), do: %{}

  defp decode_assigns(encoded) do
    encoded |> Base.decode64!() |> :erlang.binary_to_term()
  end

  defp push_url_update(url, replace) do
    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        const event = new CustomEvent("llv:navigate", {
          detail: { href: args.url, replace: args.replace },
          cancelable: true,
        });

        window.dispatchEvent(event);
      }
      """,
      %{url: url, replace: replace}
    )
  end
end
