defmodule LocalLiveView.Proxy do
  @moduledoc false
  # A LiveView implementation that wraps each Local LiveView.

  use Phoenix.LiveView

  alias Phoenix.LiveView.Socket

  @impl true
  def mount(params, session, socket) do
    llv = session["llv"]
    view = resolve_view_module!(llv.view)

    socket =
      socket
      |> put_private(:llv_view, view)
      |> put_private(:llv_id, llv.id)
      |> put_private(:mirror_id, llv.mirror_id)
      |> render_with(&view.render/1)

    {socket, opts} = call_mount(view, params, session, socket)

    assigns = decode_assigns(LocalLiveView.Dispatcher.current_assigns(llv.id))
    url = LocalLiveView.Dispatcher.current_url()

    socket =
      socket
      |> call_update!(assigns)
      |> put_server_assigns(assigns)
      |> call_handle_params(query_params(url), url)

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
    {:noreply, socket |> call_update!(assigns) |> put_server_assigns(assigns)}
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
    {:noreply, call_handle_params(socket, query_params(url), url)}
  end

  def handle_info({:llv, :patch, to, kind}, socket) do
    push_url_update(to, kind == :replace)
    {:noreply, call_handle_params(socket, query_params(to), to)}
  end

  def handle_info(msg, socket) do
    view(socket).handle_info(msg, socket)
  end

  ## Proxying helpers

  defp view(%Socket{private: %{llv_view: view}}), do: view

  defp call_mount(view, params, session, socket) do
    if function_exported?(view, :mount, 3) do
      case view.mount(params, session, socket) do
        {:ok, %Socket{} = socket} ->
          {socket, []}

        {:ok, %Socket{} = socket, opts} ->
          {socket, opts}

        other ->
          raise ArgumentError, """
          invalid return from #{inspect(view)}.mount/3 callback.

          Expected {:ok, socket} or {:ok, socket, opts}, got: #{inspect(other)}
          """
      end
    else
      {socket, []}
    end
  end

  defp call_handle_params(%Socket{} = socket, params, url) do
    view = view(socket)

    if function_exported?(view, :handle_params, 3) do
      case view.handle_params(params, url, socket) do
        {:noreply, %Socket{} = socket} ->
          socket

        other ->
          raise ArgumentError, """
          invalid return from #{inspect(view)}.handle_params/3 callback.

          Expected {:noreply, socket}, got: #{inspect(other)}
          """
      end
    else
      socket
    end
  end

  defp call_update!(%Socket{} = socket, assigns) do
    view = view(socket)

    case view.update(assigns, socket) do
      {:ok, %Socket{} = socket} ->
        socket

      other ->
        raise ArgumentError, """
        expected #{inspect(view)}.update/2 to return {:ok, %Socket{}}, got:

        #{inspect(other)}
        """
    end
  end

  defp resolve_view_module!(name) do
    module = Module.concat([name])
    loaded? = match?({:module, _module}, Code.ensure_loaded(module))

    if not loaded? or not function_exported?(module, :render, 1) do
      raise ArgumentError,
            "#{inspect(module)} (view #{inspect(name)}) is not a LocalLiveView — " <>
              "no such module, or it does not export render/1"
    end

    module
  end

  # Last assigns received from the host, for handle_push_error.
  defp put_server_assigns(socket, assigns) do
    put_private(socket, :llv_server_assigns, assigns)
  end

  # Query params are always derived from the URL they accompany — the same
  # parse for mount (create- or join-time URL) and live patches.
  defp query_params(nil), do: %{}

  defp query_params(url) do
    case String.split(url, "?", parts: 2) do
      [_path, query] -> URI.decode_query(query)
      [_path] -> %{}
    end
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
