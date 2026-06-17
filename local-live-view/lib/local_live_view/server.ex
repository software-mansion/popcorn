defmodule LocalLiveView.Message do
  @moduledoc false
  defstruct topic: nil, event: nil, payload: nil, ref: nil, join_ref: nil
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
  def handle_info({LocalLiveView.Server, params, from, phx_socket}, _ref) do
    try do
      mount(params, from, phx_socket)
    rescue
      e ->
        reraise(e, __STACKTRACE__)
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
      component_handle_event(state, cid, event, val, msg.ref)
    else
      state.socket
      |> view_handle_event(event, val)
      |> handle_result({:handle_event, 3, msg.ref}, state)
    end
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
    # The host LiveView re-rendered with new assigns for a popconent: run its
    # update/2 and push the resulting diff. Only a popconent receives assigns this
    # way (a full LocalLiveView gets its state from the mirror channel), so an
    # update targeting a non-popconent means something is wired wrong.
    unless popconent?(socket.view) do
      raise ArgumentError,
            "received an assigns update for #{inspect(socket.view)}, which is not a " <>
              "LocalLiveView.Popconent — only popconents receive assigns from a host LiveView."
    end

    case update_popconent(socket.view, assigns, socket) do
      {:ok, %Socket{} = new_socket} ->
        handle_changed(state, new_socket, nil)

      other ->
        raise ArgumentError, """
        expected #{inspect(socket.view)}.update/2 to return {:ok, %Socket{}}, got:

        #{inspect(other)}
        """
    end
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

  defp component_handle_event(state, cid, event, val, ref) do
    %{socket: socket, components: components} = state

    result =
      Diff.write_component(socket, cid, components, fn component_socket, component ->
        socket = do_handle_event(component, component_socket, event, val)
        {socket, nil}
      end)

    case result do
      {diff, new_components, _extra} ->
        {:noreply, push_diff(%{state | components: new_components}, diff, ref)}

      :error ->
        {:noreply, push_noop(state, ref)}
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

  # A popconent (`use LocalLiveView.Popconent`) is driven by mount/1 + update/2,
  # unlike a full LocalLiveView's mount/3. Detected by the __popconent__/0 marker
  # the macro injects — a function check, since @behaviour reflection isn't
  # available in the AtomVM runtime this server runs in.
  defp popconent?(view), do: function_exported?(view, :__popconent__, 0)

  # Popconent mount lifecycle (it has no mount/3): run mount/1, then feed the
  # initial assigns through update/2. Returns the mounted %Socket{}.
  defp mount_popconent(view, assigns, %Socket{} = socket) do
    with {:ok, %Socket{} = socket} <- view.mount(socket),
         {:ok, %Socket{} = socket} <- update_popconent(view, assigns, socket) do
      socket
    else
      other ->
        raise ArgumentError, """
        expected #{inspect(view)}.mount/1 and update/2 to return {:ok, %Socket{}}, got:

        #{inspect(other)}
        """
    end
  end

  # Re-run the popconent's update/2 for the (JSON, string-keyed) assigns the host
  # pushed.
  defp update_popconent(view, assigns, %Socket{} = socket) do
    view.update(normalize_assigns(assigns), socket)
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

  defp maybe_call_mount_handle_params(%{socket: socket} = state, params) do
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
        |> Utils.call_handle_params!(view, lifecycle.exported?, params)
        |> mount_handle_params_result(state, :mount)
    end
  end

  defp mount_handle_params_result({:noreply, %Socket{} = new_socket}, state, redir) do
    new_state = %{state | socket: new_socket}

    case maybe_diff(new_state, true) do
      {:diff, diff, new_state} ->
        {:ok, diff, redir, new_state}
    end
  end

  defp handle_result({:noreply, %Socket{} = new_socket}, {_from, _arity, ref}, state) do
    handle_changed(state, new_socket, ref)
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

  defp handle_changed(state, %Socket{} = new_socket, ref, pending_live_patch \\ nil) do
    new_state = %{state | socket: new_socket}

    case maybe_diff(new_state, false) do
      {:diff, diff, new_state} ->
        {:noreply,
         new_state
         |> clear_live_patch_counter()
         |> push_live_patch(pending_live_patch)
         |> push_diff(diff, ref)}

      _result ->
        state
    end
  end

  defp clear_live_patch_counter(state) do
    %{state | redirect_count: 0}
  end

  defp push_live_patch(state, nil), do: state
  defp push_live_patch(state, opts), do: push(state, "live_patch", opts)

  defp push_noop(state, nil = _ref), do: state
  defp push_noop(state, ref), do: reply(state, ref, :ok, %{})

  defp push_diff(state, diff, ref) when diff == %{}, do: push_noop(state, ref)
  defp push_diff(state, diff, nil = _ref), do: push(state, "diff", diff)
  defp push_diff(state, diff, ref), do: reply(state, ref, :ok, %{diff: diff})

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

    new_socket = Utils.clear_temp(socket)

    {:diff, diff,
     %{state | socket: new_socket, fingerprints: fingerprints, components: components}}
  end

  def rerender(socket) do
    Phoenix.LiveView.Renderer.to_rendered(socket, socket.view)
  end

  defp reply(state, {ref, extra}, status, payload) do
    reply(state, ref, status, Map.merge(payload, extra))
  end

  defp reply(state, _ref, _status, %{diff: diff}) do
    push(state, "diff", diff)
  end

  defp reply(state, _ref, _status, _payload), do: state

  defp push(state, "diff", diff) do
    llv_id = state.llv_id

    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        window.__popcornTransportReceive(args.id, args.diff);
      }
      """,
      %{id: llv_id, diff: diff}
    )

    state
  end

  defp push(state, _event, _payload), do: state

  ## Mount

  defp mount(%{"session" => session} = params, from, phx_socket) do
    with %Phoenix.LiveView.Session{view: view} <- session,
         {:ok, config} <- load_live_view(view) do
      verified_mount(
        session,
        config,
        params,
        from,
        phx_socket
      )
    else
      {:error, _reason} ->
        GenServer.reply(from, {:error, %{reason: "stale"}})
        {:stop, :shutdown, :no_state}
    end
  end

  defp mount(%{}, from, phx_socket) do
    Logger.error("Mounting #{phx_socket.topic} failed because no session was provided")
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

  #
  defp verified_mount(
         %Session{} = verified,
         config,
         params,
         from,
         phx_socket
       ) do
    %Session{
      view: view
    } = verified

    # Assigns passed via the `<.popconent assigns... />` component reach us
    # here as a string-keyed map (empty for a plain local view). They become the
    # first argument to the view's mount/3 callback — `LocalLiveView.Popconent` routes them
    # on to its update/2.
    initial_assigns = params["assigns"] || %{}
    connect_params = initial_assigns
    llv_id = params["llv_id"]

    socket = %Socket{
      view: view
    }

    lifecycle = load_lifecycle(config, nil)

    case mount_private(verified, connect_params, nil, lifecycle) do
      {:ok, mount_priv} ->
        socket = %{
          socket
          | id: "phx-",
            private: Map.put(mount_priv, :llv_id, llv_id),
            assigns: Map.merge(socket.assigns, %{live_action: nil, flash: nil}),
            host_uri: :not_mounted_at_router
        }

        try do
          mounted_socket = %Socket{socket | view: view}

          # A popconent has no mount/3; its lifecycle (mount/1 + update/2 seeded
          # with the initial assigns) is driven here. A full LocalLiveView goes
          # through the standard mount/3 path.
          mounted_socket =
            if popconent?(view) do
              mount_popconent(view, initial_assigns, mounted_socket)
            else
              Utils.maybe_call_live_view_mount!(mounted_socket, view, initial_assigns, verified)
            end

          mounted_socket
          |> build_state(phx_socket, llv_id)
          |> maybe_call_mount_handle_params(initial_assigns)
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
        GenServer.reply(from, {:ok, reply})
        {:noreply, post_verified_mount(new_state)}

      {:ok, diff, {:live_patch, opts}, new_state} ->
        reply =
          put_container(session, nil, %{
            rendered: diff,
            live_patch: opts,
            liveview_version: lv_vsn
          })

        GenServer.reply(from, {:ok, reply})
        {:noreply, post_verified_mount(new_state)}

      {:live_redirect, opts, new_state} ->
        GenServer.reply(from, {:error, %{live_redirect: opts}})
        {:stop, :shutdown, new_state}

      {:redirect, opts, new_state} ->
        GenServer.reply(from, {:error, %{redirect: opts}})
        {:stop, :shutdown, new_state}
    end
  end

  defp build_state(%Socket{} = lv_socket, %Phoenix.Socket{} = phx_socket, llv_id) do
    %{
      join_ref: phx_socket.join_ref,
      serializer: phx_socket.serializer,
      socket: lv_socket,
      topic: phx_socket.topic,
      components: Diff.new_components(),
      fingerprints: Diff.new_fingerprints(),
      redirect_count: 0,
      upload_names: %{},
      upload_pids: %{},
      llv_id: llv_id
    }
  end

  defp post_verified_mount(%{socket: socket} = state) do
    %{state | socket: Utils.post_mount_prune(socket)}
  end
end
