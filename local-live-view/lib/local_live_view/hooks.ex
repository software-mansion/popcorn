defmodule LocalLiveView.Hooks do
  @moduledoc false
  # The LLV-specific semantics, expressed through regular LiveView lifecycle
  # mechanisms so the stock Phoenix.LiveView.Channel can host local views
  # unchanged:
  #
  #  * mount/4 wraps the view's own mount/3 (injected by
  #    LocalLiveView.__before_compile__): it stores the LLV id in
  #    socket.private, attaches the handle_info hook below, feeds the
  #    host-passed assigns through update/2 (LiveComponent-style) and runs the
  #    initial handle_params — everything LocalLiveView adds on top of a plain
  #    LiveView mount.
  #  * handle_info/2 intercepts the {:llv, ...} messages the Dispatcher sends
  #    for interactions that are not channel frames (host assign updates,
  #    failed server pushes, mirror reconnects, navigation).

  alias Phoenix.LiveView.Socket

  def mount(params, session, socket, orig_mount) do
    socket =
      socket
      |> Phoenix.LiveView.put_private(:llv_id, session["llv_id"])
      |> Phoenix.LiveView.attach_hook(:llv_internal, :handle_info, &handle_info/2)

    {socket, opts} = call_orig_mount(orig_mount, params, session, socket)

    assigns = normalize_assigns(session["assigns"] || %{})

    socket =
      socket
      |> call_update!(assigns)
      |> put_server_assigns(assigns)
      |> call_handle_params(session["url_params"] || %{}, session["url"])

    case opts do
      [] -> {:ok, socket}
      opts -> {:ok, socket, opts}
    end
  end

  defp call_orig_mount(nil, _params, _session, socket), do: {socket, []}

  defp call_orig_mount(orig_mount, params, session, socket) do
    case orig_mount.(params, session, socket) do
      {:ok, %Socket{} = socket} ->
        {socket, []}

      {:ok, %Socket{} = socket, opts} ->
        {socket, opts}

      other ->
        raise ArgumentError, """
        invalid return from #{inspect(socket.view)}.mount/3 callback.

        Expected {:ok, socket} or {:ok, socket, opts}, got: #{inspect(other)}
        """
    end
  end

  # The host LiveView re-rendered with new assigns for this view: run its
  # update/2. data-pop-assigns carries the full set of host-forwarded assigns
  # each render, so it replaces the stored server_assigns.
  def handle_info({:llv, :update_assigns, raw_assigns}, socket) do
    assigns = normalize_assigns(raw_assigns)
    {:halt, socket |> call_update!(assigns) |> put_server_assigns(assigns)}
  end

  # A push_server_event failed to reach the host: hand the view the last
  # assigns received from it, so it can roll back to authoritative state.
  def handle_info({:llv, :push_error, event, params}, socket) do
    server_assigns = socket.private[:llv_server_assigns] || %{}

    case socket.view.handle_push_error(event, params, server_assigns, socket) do
      {:noreply, %Socket{} = socket} ->
        {:halt, socket}

      other ->
        raise ArgumentError, """
        invalid return from #{inspect(socket.view)}.handle_push_error/4 callback.

        Expected {:noreply, socket}, got: #{inspect(other)}
        """
    end
  end

  # The mirror channel (re)joined: sync every assign to the server.
  def handle_info({:llv, :reconnected}, socket) do
    LocalLiveView.mirror_sync(socket, Map.keys(socket.assigns))
    {:halt, socket}
  end

  # Browser-side navigation (patch links, popstate, host phx:navigate).
  def handle_info({:llv, :handle_params, params, url}, socket) do
    {:halt, call_handle_params(socket, params, url)}
  end

  # LocalLiveView.push_patch: write the browser URL, then run handle_params —
  # mirroring Phoenix live-patch semantics without router machinery.
  def handle_info({:llv, :patch, to, kind}, socket) do
    push_url_update(to, kind == :replace)

    params =
      case String.split(to, "?", parts: 2) do
        [_path, query] -> URI.decode_query(query)
        [_path] -> %{}
      end

    {:halt, call_handle_params(socket, params, to)}
  end

  def handle_info(_msg, socket), do: {:cont, socket}

  # The view's handle_params/3 is renamed to __llv_handle_params__/3 at compile
  # time (see LocalLiveView.__rewrite_handle_params__): Phoenix.LiveView.Channel
  # refuses to mount a router-less view that exports handle_params/3, so LLV
  # hides the export and drives the callback itself.
  defp call_handle_params(%Socket{} = socket, params, url) do
    view = socket.view

    if function_exported?(view, :__llv_handle_params__, 3) do
      case view.__llv_handle_params__(params, url, socket) do
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
    view = socket.view

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

  # Last (normalized) assigns received from the host, for handle_push_error.
  defp put_server_assigns(socket, assigns) do
    Phoenix.LiveView.put_private(socket, :llv_server_assigns, assigns)
  end

  # Top-level assign keys cross the JSON boundary as strings; convert them back
  # to atoms so they read like Phoenix assigns (`@items`). Nested values are
  # left as is — deeply atomizing arbitrary maps would be unsafe.
  def normalize_assigns(assigns) do
    Map.new(assigns, fn
      {key, value} when is_atom(key) -> {key, value}
      {key, value} when is_binary(key) -> {String.to_atom(key), value}
    end)
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
