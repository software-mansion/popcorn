defmodule LocalLiveView.Proxy do
  @moduledoc false
  # The single real Phoenix.LiveView that every local view runs as.
  #
  # The container the dispatcher renders for each view (see render_container)
  # names this module as the LiveView in its session token, so the channel
  # always mounts it; the actual `use LocalLiveView` module travels in the
  # token's session (the :view key under "llv") and every callback is proxied to it at
  # runtime. This keeps local view modules plain implementations of the
  # LocalLiveView behaviour — no generated __live__/0, no compile-time mount
  # wrapping.
  #
  # handle_params/3 is deliberately not exported here: the channel refuses to
  # mount a router-less view that exports it. The proxy invokes the view's
  # handle_params/3 itself — at mount and on the {:llv, ...} navigation
  # messages below.

  use Phoenix.LiveView

  alias Phoenix.LiveView.Socket

  # The session contract. Assembled by the dispatcher's create clause from
  # the bare wire values the JS side sends (Views.mount in views.ts),
  # passed into live_render — which signs it into the container's join
  # token — and read only by mount/3 below. LiveView requires the session's
  # top-level keys to be strings, so everything rides in one atom-keyed
  # map under the "llv" key (values are opaque signed terms). Its keys:
  #
  #   :view      - the view module name, as written in data-pop-view
  #   :id        - the mount point's element id (also the channel topic id)
  #   :epoch     - the incarnation epoch the DISPATCHER mints at create;
  #                carried back in register_channel so registration is
  #                incarnation-exact
  #   :mirror_id - the server-mirror identity; nil without a mirror
  #
  # The URL and the host assigns deliberately do not travel here: mount
  # reads both from the dispatcher's ETS (current_url/current_assigns),
  # seeded before this view's create and kept fresh by url_changed/
  # update_assigns — so remounts never resurrect create-time state.

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

    # Both mount inputs are read from the dispatcher's ETS so this — and
    # every remount — runs with the freshest values BEFORE the initial
    # render (messages can't get here in time; they arrive only after
    # registration). Neither can be stale: the URL is reported and the
    # assigns cache seeded before this view's create, both ahead of any
    # join in the FIFO; nil assigns means a mount point without them.
    assigns = decode_assigns(LocalLiveView.Dispatcher.current_assigns(llv.id))
    url = LocalLiveView.Dispatcher.current_url()

    socket =
      socket
      |> call_update!(assigns)
      |> put_server_assigns(assigns)
      |> call_handle_params(query_params(url), url)

    # Mount succeeded: tell the dispatcher this process is the channel for
    # this incarnation of the id, so it can route out-of-band messages here.
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

  # The {:llv, ...} messages are sent by the Dispatcher (and by
  # LocalLiveView.push_patch) for interactions that are not channel frames:
  # host assign updates, server messages, failed server pushes, mirror
  # reconnects, navigation. Everything else belongs to the view's
  # handle_info/2.

  # The host LiveView re-rendered with new assigns for this view: run its
  # update/2. data-pop-assigns carries the full set of host-forwarded assigns
  # each render, so it replaces the stored server_assigns.
  @impl true
  def handle_info({:llv, %{"action" => "update_assigns", "assigns" => encoded_assigns}}, socket) do
    assigns = decode_assigns(encoded_assigns)
    {:noreply, socket |> call_update!(assigns) |> put_server_assigns(assigns)}
  end

  # A server message pushed by the host (push_event("llv_server_message"),
  # forwarded by LLVEngine through the dispatcher, which queues it while no
  # channel is joined). The callback's spec allows {:reply, map, socket} —
  # via this delivery there is no push to reply to, so the reply is dropped.
  def handle_info(
        {:llv, %{"action" => "server_message", "params" => %{"type" => type} = params}},
        socket
      ) do
    case view(socket).handle_server_event(type, params, socket) do
      {:noreply, %Socket{} = socket} -> {:noreply, socket}
      {:reply, _map, %Socket{} = socket} -> {:noreply, socket}
    end
  end

  # LLVEngine.pushEvent, the external JS API: runs the view's handle_event/3
  # exactly like a DOM event would, but delivered through the dispatcher —
  # queued during the construction window, refused ("view is dead") in the
  # crash → rejoin window. The render reaches the browser as an out-of-band
  # diff rather than an ack; a {:reply, ...} return has no push to reply to
  # and is dropped.
  def handle_info(
        {:llv, %{"action" => "push_event", "event" => event, "params" => params}},
        socket
      ) do
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

  # A push_server_event failed to reach the host: hand the view the last
  # assigns received from it, so it can roll back to authoritative state.
  def handle_info(
        {:llv, %{"action" => "push_error", "event" => event, "params" => params}},
        socket
      ) do
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

  # The mirror channel (re)joined: sync every assign to the server.
  def handle_info({:llv, %{"action" => "mirror_reconnected"}}, socket) do
    LocalLiveView.mirror_sync(socket, Map.keys(socket.assigns))
    {:noreply, socket}
  end

  # Browser-side navigation this view did not initiate (patch links,
  # popstate, host phx:navigate), fanned out by the dispatcher's
  # "navigated" clause to every view with a live channel.
  def handle_info({:llv, %{"action" => "handle_params", "url" => url}}, socket) do
    {:noreply, call_handle_params(socket, query_params(url), url)}
  end

  # LocalLiveView.push_patch: write the browser URL, then run handle_params —
  # mirroring Phoenix live-patch semantics without router machinery.
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

  # The session carries the view as the name written in data-pop-view;
  # resolve it to the module and make sure it is a local view. Raising here
  # crashes the join: the browser sees error acks until stock LiveView gives
  # up (MAX_CHILD_JOIN_ATTEMPTS) — a dev-time error, so the message carries
  # the names needed to act on it.
  defp resolve_view_module!(name) do
    module = String.to_atom("Elixir." <> name)

    # Verified on AtomVM (in-browser probe, 2026-08-31): Code.ensure_loaded/1
    # returns {:module, m} for bundle modules and {:error, :embedded} for
    # unknown ones — no raise. On the BEAM (host trace harness) it
    # force-loads, so function_exported?/3 is meaningful on both runtimes.
    loaded? = match?({:module, _module}, Code.ensure_loaded(module))

    unless loaded? and function_exported?(module, :render, 1) do
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

  # Host-forwarded assigns cross the boundary as base64(term_to_binary(map)) —
  # the counterpart of LocalLiveView.Component.encode_assigns — carried
  # verbatim through the JS hook and the dispatcher, so keys round-trip
  # exactly as encoded (atom keys of a HEEx assigns map); nil means the mount
  # point rendered without data-pop-assigns.
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
