defmodule LocalLiveView.Message do
  @moduledoc false
  # `promise` is the popcorn.call promise of the browser frame this message
  # answers (settled via Popcorn.Wasm.resolve when processed); nil for
  # uncorrelated sends.
  defstruct event: nil, payload: nil, promise: nil
end

defmodule LocalLiveView.Server do
  @moduledoc false
  #  A LocalLiveView.Server is a process that receives events, updates
  #  its state, and renders updates to a page as diffs.
  #
  #  LocalLiveView.Server is a instance of GenServer that handles
  #  events for LocalLiveView, just like a Phoenix.Channel.
  #
  #  One LocalLiveView.Server will be created per every LocalLiveView
  #  used on the page.

  use GenServer, restart: :temporary

  require Logger

  alias Phoenix.LiveView.{
    Socket,
    Utils,
    Diff,
    Session,
    Lifecycle
  }

  alias Phoenix.LiveView.Socket
  alias Phoenix.LiveView.Diff

  alias LocalLiveView.Message

  @doc """
  Starts LocalLiveView.Server process.
  """
  def start_llv_process() do
    DynamicSupervisor.start_child(
      LocalLiveView.Server.Supervisor,
      __MODULE__.child_spec([])
    )
  end

  @doc false
  def start_link(_arg) do
    GenServer.start_link(__MODULE__, [])
  end

  @impl true
  def init([]) do
    {:ok, []}
  end

  @impl true
  def handle_info({LocalLiveView.Server, params, from}, _ref) do
    try do
      mount(params, from)
    rescue
      e ->
        reraise(e, __STACKTRACE__)
    end
  end

  def handle_info(
        %Message{event: "handle_params", payload: %{"params" => params, "url" => url}},
        %{socket: socket} = state
      ) do
    view = socket.view
    lifecycle = Lifecycle.stage_info(socket, view, :handle_params, 3)

    if lifecycle.any? do
      socket
      |> call_handle_params(view, lifecycle.exported?, params, url)
      |> handle_result({:handle_params, 3, nil}, state)
    else
      {:noreply, state}
    end
  end

  def handle_info(%Message{event: "llv_reconnected"}, state) do
    keys = Map.keys(state.socket.assigns)
    LocalLiveView.mirror_sync(state.socket, keys)
    {:noreply, state}
  end

  def handle_info(%Message{event: "event"} = msg, state) do
    %{"value" => raw_val, "event" => event, "type" => type} = msg.payload
    val = decode_event_type(type, raw_val, msg.payload)

    if cid = msg.payload["cid"] do
      component_handle_event(state, cid, event, val, msg.promise)
    else
      state.socket
      |> view_handle_event(event, val)
      |> handle_result({:handle_event, 3, msg.promise}, state)
    end
  end

  def handle_info(%Message{event: "phx_join"} = msg, state) do
    # The channel join ack carries the rendered produced at mount. On a rejoin
    # (channel error recovery) that's already consumed — serve a fresh full
    # render instead, resetting diff tracking to match the client's clean
    # slate.
    {rendered, state} =
      case state.initial_rendered do
        nil ->
          state = %{
            state
            | fingerprints: Diff.new_fingerprints(),
              components: Diff.new_components()
          }

          {:diff, diff, state} = render_diff(state, state.socket, true)
          {diff, state}

        rendered ->
          {rendered, %{state | initial_rendered: nil}}
      end

    {:noreply, reply(state, msg.promise, :ok, %{rendered: rendered})}
  end

  # Component-destruction bookkeeping, mirroring Phoenix.LiveView.Channel: the
  # browser announces cids about to leave the DOM, then confirms removal. Without
  # the prune, state.components would retain dead component state forever.
  def handle_info(
        %Message{event: "cids_will_destroy", payload: %{"cids" => cids}} = msg,
        state
      ) do
    components =
      Enum.reduce(cids, state.components, &Diff.mark_for_deletion_component/2)

    {:noreply, reply(%{state | components: components}, msg.promise, :ok, %{})}
  end

  def handle_info(
        %Message{event: "cids_destroyed", payload: %{"cids" => cids}} = msg,
        state
      ) do
    {deleted_cids, components} =
      Enum.flat_map_reduce(cids, state.components, &Diff.delete_component/2)

    # The ack carries the cids actually deleted; the browser prunes exactly
    # those from its rendered tree (a cid re-added mid-flight is kept).
    {:noreply, reply(%{state | components: components}, msg.promise, :ok, %{cids: deleted_cids})}
  end

  def handle_info(
        %Message{event: "js_push", payload: %{"event" => event, "payload" => payload}},
        %{socket: socket} = state
      ) do
    {:js_push, event, payload}
    |> view_handle_info(socket)
    |> handle_result({:handle_info, 2, nil}, state)
  end

  def handle_info(%Message{event: "update_assigns", payload: assigns}, %{socket: socket} = state) do
    # The host LiveView re-rendered with new assigns for this view: run its
    # update/2 and push the resulting diff. data-pop-assigns carries the full
    # set of host-forwarded assigns each render, so it replaces server_assigns.
    assigns = normalize_assigns(assigns)
    new_socket = call_update!(socket.view, assigns, socket)
    handle_changed(%{state | server_assigns: assigns}, new_socket, nil)
  end

  def handle_info(
        %Message{event: "push_error", payload: %{"event" => event, "payload" => params}},
        %{socket: socket} = state
      ) do
    # A push_server_event failed to reach the host: hand the view the last
    # assigns received from it, so it can roll back to authoritative state.
    # `params` is the exact map the view passed to push_server_event.
    event
    |> socket.view.handle_push_error(params, state.server_assigns, socket)
    |> handle_result({:handle_push_error, 4, nil}, state)
  end

  def handle_info({:phoenix, :send_update, update}, state) do
    case Diff.update_component(state.socket, state.components, update) do
      {diff, new_components} ->
        {:noreply, push_diff(%{state | components: new_components}, diff, nil)}

      :noop ->
        {:noreply, state}
    end
  end

  def handle_info(msg, %{socket: socket} = state) do
    msg
    |> view_handle_info(socket)
    |> handle_result({:handle_info, 2, nil}, state)
  end

  defp view_handle_event(%Socket{} = socket, event, val) do
    {:noreply, do_handle_event(socket.view, socket, event, val)}
  end

  defp component_handle_event(state, cid, event, val, promise) do
    %{socket: socket, components: components} = state

    result =
      Diff.write_component(socket, cid, components, fn component_socket, component ->
        socket = do_handle_event(component, component_socket, event, val)
        {socket, nil}
      end)

    case result do
      {diff, new_components, _extra} ->
        {:noreply, push_diff(%{state | components: new_components}, diff, promise)}

      :error ->
        {:noreply, push_noop(state, promise)}
    end
  end

  defp do_handle_event(_module, %Socket{} = socket, "lv:clear-flash", val) do
    case val do
      %{"key" => key} -> Utils.clear_flash(socket, key)
      _val -> Utils.clear_flash(socket)
    end
  end

  defp do_handle_event(_module, %Socket{}, "lv:" <> _ = bad_event, _val) do
    raise ArgumentError, """
    Received unknown LiveView event #{inspect(bad_event)}.
    The following LiveView events are supported: lv:clear-flash.
    """
  end

  defp do_handle_event(module, %Socket{} = socket, event, val) do
    case module.handle_event(event, val, socket) do
      {:noreply, %Socket{} = socket} -> socket
      {:reply, %{} = reply, %Socket{} = socket} -> Utils.put_reply(socket, reply)
      other -> raise_bad_callback_response!(other, module, :handle_event, 3)
    end
  end

  defp view_handle_info(msg, %{view: view} = socket) do
    view.handle_info(msg, socket)
  end

  # Run the view's update/2 for the host-passed assigns (already normalized by
  # the caller), raising on a bad return. Returns the updated %Socket{}.
  defp call_update!(view, assigns, %Socket{} = socket) do
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

  # Top-level assign keys cross the JSON boundary as strings; convert them back to
  # atoms so they read like Phoenix assigns (`@items`). Nested values are left as
  # is — deeply atomizing arbitrary maps would be unsafe.
  defp normalize_assigns(assigns) do
    Map.new(assigns, fn
      {key, value} when is_atom(key) -> {key, value}
      {key, value} when is_binary(key) -> {String.to_atom(key), value}
    end)
  end

  defp decode_event_type("form", url_encoded, raw_payload) do
    url_encoded
    |> Plug.Conn.Query.decode()
    |> maybe_merge_meta(raw_payload)
    |> decode_merge_target()
  end

  defp decode_event_type(_, value, _raw_payload), do: value

  defp decode_merge_target(%{"_target" => target} = params) when is_list(target), do: params

  defp decode_merge_target(%{"_target" => target} = params) when is_binary(target) do
    keyspace = target |> Plug.Conn.Query.decode() |> gather_keys([])
    Map.put(params, "_target", Enum.reverse(keyspace))
  end

  defp decode_merge_target(%{} = params), do: params

  defp maybe_merge_meta(value, %{"meta" => meta}) when is_map(value) do
    Map.merge(value, meta)
  end

  defp maybe_merge_meta(value, _raw_payload), do: value

  defp gather_keys(%{} = map, acc) do
    case Enum.at(map, 0) do
      {key, val} -> gather_keys(val, [key | acc])
      nil -> acc
    end
  end

  defp gather_keys([], acc), do: acc
  defp gather_keys([%{} = map], acc), do: gather_keys(map, acc)
  defp gather_keys(_, acc), do: acc

  defp maybe_call_mount_handle_params(%{socket: socket} = state, %{
         "url_params" => url_params,
         "url" => url
       }) do
    %{view: view, redirected: mount_redirect} = socket
    lifecycle = Lifecycle.stage_info(socket, view, :handle_params, 3)

    cond do
      mount_redirect ->
        mount_handle_params_result({:noreply, socket}, state, :mount)

      not lifecycle.any? ->
        {:diff, diff, new_state} = render_diff(state, socket, true)
        {:ok, diff, :mount, new_state}

      true ->
        socket
        |> call_handle_params(view, lifecycle.exported?, url_params, url)
        |> mount_handle_params_result(state, :mount)
    end
  end

  defp call_handle_params(socket, view, true, params, url) do
    case view.handle_params(params, url, socket) do
      {:noreply, %Socket{} = socket} -> {:noreply, socket}
      other -> raise_bad_callback_response!(other, view, :handle_params, 3)
    end
  end

  defp call_handle_params(socket, _view, false, _params, _url) do
    {:noreply, socket}
  end

  defp mount_handle_params_result({:noreply, %Socket{} = new_socket}, state, redir) do
    new_state = %{state | socket: new_socket}

    case maybe_diff(new_state, true) do
      {:diff, diff, new_state} ->
        {:ok, diff, redir, new_state}
    end
  end

  defp handle_result({:noreply, %Socket{} = new_socket}, {_from, _arity, promise}, state) do
    handle_changed(state, new_socket, promise)
  end

  defp handle_result(result, {name, arity, _ref}, state) do
    raise_bad_callback_response!(result, state.socket.view, name, arity)
  end

  defp raise_bad_callback_response!(result, view, :handle_call, 3) do
    raise ArgumentError, """
    invalid noreply from #{inspect(view)}.handle_call/3 callback.

    Expected one of:

        {:noreply, %Socket{}}
        {:reply, map, %Socket{}}

    Got: #{inspect(result)}
    """
  end

  defp raise_bad_callback_response!(result, view, :handle_event, arity) do
    raise ArgumentError, """
    invalid return from #{inspect(view)}.handle_event/#{arity} callback.

    Expected one of:

        {:noreply, %Socket{}}
        {:reply, map, %Socket{}}

    Got: #{inspect(result)}
    """
  end

  defp raise_bad_callback_response!(result, view, name, arity) do
    raise ArgumentError, """
    invalid noreply from #{inspect(view)}.#{name}/#{arity} callback.

    Expected one of:

        {:noreply, %Socket{}}

    Got: #{inspect(result)}
    """
  end

  defp handle_changed(state, %Socket{} = new_socket, promise) do
    new_state = %{state | socket: new_socket}

    case maybe_diff(new_state, false) do
      {:diff, diff, new_state} ->
        {:noreply,
         new_state
         |> clear_live_patch_counter()
         |> push_diff(diff, promise)}

      {:live, :patch, opts} ->
        handle_live_patch(new_state, opts, promise)

      _result ->
        {:noreply, state}
    end
  end

  defp handle_live_patch(state, opts, promise) do
    to = opts.to
    replace = opts.kind == :replace

    url_params =
      case String.split(to, "?", parts: 2) do
        [_path, query] -> URI.decode_query(query)
        [_path] -> %{}
      end

    socket = %{state.socket | redirected: nil}
    new_state = %{state | socket: socket}

    view = socket.view
    lifecycle = Lifecycle.stage_info(socket, view, :handle_params, 3)

    push_url_update(to, replace)

    if lifecycle.any? do
      socket
      |> call_handle_params(view, lifecycle.exported?, url_params, to)
      |> handle_result({:handle_params, 3, promise}, new_state)
    else
      {:noreply, new_state}
    end
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

  defp clear_live_patch_counter(state) do
    %{state | redirect_count: 0}
  end

  defp push_noop(state, nil = _promise), do: state
  defp push_noop(state, promise), do: reply(state, promise, :ok, %{})

  defp push_diff(state, diff, promise) when diff == %{}, do: push_noop(state, promise)

  # Out-of-band diff (handle_info renders, update_assigns, push_error
  # rollbacks, server messages — nothing answering a browser frame): injected
  # browser-side as a channel "diff" frame, handled exactly like a
  # server-pushed diff.
  defp push_diff(state, diff, nil = _promise) do
    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        window.__popcornTransportReceive(args.id, args.diff);
      }
      """,
      %{id: state.llv_id, diff: diff}
    )

    state
  end

  defp push_diff(state, diff, promise), do: reply(state, promise, :ok, %{diff: diff})

  defp maybe_diff(%{socket: socket} = state, force?) do
    socket.redirected || render_diff(state, socket, force?)
  end

  defp render_diff(state, socket, force?) do
    changed? = Utils.changed?(socket)

    {socket, diff, fingerprints, components} =
      if force? or changed? do
        rendered = rerender(socket)

        {diff, fingerprints, components} =
          Diff.render(socket, rendered, state.fingerprints, state.components)

        socket =
          socket
          |> Utils.clear_changed()

        {socket, diff, fingerprints, components}
      else
        {socket, %{}, state.fingerprints, state.components}
      end

    # Same as Phoenix.LiveView.Channel: fold the handle_event reply (:r) and
    # push_event events (:e) into the diff — even when nothing re-rendered —
    # so the browser's Rendered.extract delivers them. clear_temp below
    # ensures they ride at most one diff.
    diff = Diff.render_private(socket, diff)

    new_socket = Utils.clear_temp(socket)

    {:diff, diff,
     %{state | socket: new_socket, fingerprints: fingerprints, components: components}}
  end

  def rerender(socket) do
    Phoenix.LiveView.Renderer.to_rendered(socket, socket.view)
  end

  # Answer a browser frame by settling its popcorn.call promise (threaded
  # through Message.promise by the dispatcher). The browser transport turns
  # the resolved {status, payload} — %{diff: diff} or %{} for a no-op —
  # into the channel push ack, driving LiveView's stock push lifecycle:
  # apply the ack diff, undo the push's element refs, resolve the reply.
  defp reply(state, promise, status, payload) do
    Popcorn.Wasm.resolve(%{status: status, payload: payload}, promise)
    state
  end

  ## Mount

  defp mount(%{"session" => session} = params, from) do
    with %Phoenix.LiveView.Session{view: view} <- session,
         {:ok, config} <- load_live_view(view) do
      verified_mount(
        session,
        config,
        params,
        from
      )
    else
      {:error, _reason} ->
        GenServer.reply(from, {:error, %{reason: "stale"}})
        {:stop, :shutdown, :no_state}
    end
  end

  defp mount(%{} = params, from) do
    Logger.error("Mounting LocalLiveView #{inspect(params["id"])} failed because no session was provided")
    GenServer.reply(from, {:error, %{reason: "stale"}})
    {:stop, :shutdown, :no_session}
  end

  defp load_live_view(view) do
    # Make sure the view is loaded. Otherwise if the first request
    # ever is a LiveView connection, the view won't be loaded and
    # the mount/handle_params callbacks won't be invoked as they
    # are optional, leading to errors.
    {:ok, view.__live__()}
  rescue
    # If it fails, then the only possible answer is that the live
    # view has been renamed. So we force the client to reconnect.
    _ -> {:error, :stale}
  end

  defp verified_mount(
         %Session{} = verified,
         config,
         params,
         from
       ) do
    %Session{
      view: view
    } = verified

    # Assigns passed via the `<.local_live_view assigns... />` component reach us
    # here as a string-keyed map. They become the first argument to the view's
    # mount/3 callback and are then fed through its update/2.
    initial_assigns = normalize_assigns(params["assigns"] || %{})
    llv_id = params["id"]

    socket = %Socket{
      view: view
    }

    lifecycle = load_lifecycle(config, nil)

    case mount_private(verified, params, nil, lifecycle) do
      {:ok, mount_priv} ->
        socket = %{
          socket
          | id: "phx-",
            private: Map.put(mount_priv, :llv_id, llv_id),
            assigns: Map.merge(socket.assigns, %{live_action: nil, flash: nil}),
            host_uri: :not_mounted_at_router
        }

        try do
          # Run mount/3 (params are the host-passed assigns), then feed those
          # assigns through update/2, LiveComponent-style.
          mounted_socket =
            %Socket{socket | view: view}
            |> Utils.maybe_call_live_view_mount!(view, params, verified)
            |> then(&call_update!(view, initial_assigns, &1))

          mounted_socket
          |> build_state(llv_id, initial_assigns)
          |> maybe_call_mount_handle_params(params)
          |> reply_mount(from, verified)
        rescue
          exception ->
            reraise(exception, __STACKTRACE__)
        end

      {:error, :noproc} ->
        GenServer.reply(from, {:error, %{reason: "stale"}})
        {:stop, :shutdown, :no_state}
    end
  end

  defp load_lifecycle(%{lifecycle: lifecycle}, _) do
    lifecycle
  end

  defp mount_private(%Session{parent_pid: nil} = session, connect_params, connect_info, lifecycle) do
    %{
      root_view: root_view,
      assign_new: assign_new,
      live_session_name: live_session_name
    } = session

    {:ok,
     %{
       connect_params: connect_params,
       connect_info: connect_info,
       assign_new: {%{}, assign_new},
       lifecycle: lifecycle,
       root_view: root_view,
       live_temp: %{},
       live_session_name: live_session_name
     }}
  end

  defp put_container(%Session{}, nil = _route, %{} = diff), do: diff

  defp reply_mount(result, from, %Session{} = session) do
    lv_vsn = to_string(Application.spec(:phoenix_live_view)[:vsn])

    case result do
      {:ok, diff, :mount, new_state} ->
        reply = put_container(session, nil, diff)
        GenServer.reply(from, :ok)
        {:noreply, post_verified_mount(%{new_state | initial_rendered: reply})}

      {:ok, diff, {:live_patch, opts}, new_state} ->
        reply =
          put_container(session, nil, %{
            rendered: diff,
            live_patch: opts,
            liveview_version: lv_vsn
          })

        GenServer.reply(from, :ok)
        {:noreply, post_verified_mount(%{new_state | initial_rendered: reply})}

      {:live_redirect, opts, new_state} ->
        GenServer.reply(from, {:error, %{live_redirect: opts}})
        {:stop, :shutdown, new_state}

      {:redirect, opts, new_state} ->
        GenServer.reply(from, {:error, %{redirect: opts}})
        {:stop, :shutdown, new_state}
    end
  end

  defp build_state(%Socket{} = lv_socket, llv_id, server_assigns) do
    %{
      socket: lv_socket,
      components: Diff.new_components(),
      fingerprints: Diff.new_fingerprints(),
      redirect_count: 0,
      llv_id: llv_id,
      # The mount rendered, held until the channel join ack consumes it.
      initial_rendered: nil,
      # Last (normalized) assigns received from the host, for handle_push_error.
      # Updated only on mount and "update_assigns"; if the mirror channel's
      # "set_assigns" push ever gets a JS consumer, it must update this too.
      server_assigns: server_assigns
    }
  end

  defp post_verified_mount(%{socket: socket} = state) do
    %{state | socket: Utils.post_mount_prune(socket)}
  end
end
