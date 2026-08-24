defmodule LocalLiveView.Dispatcher do
  @moduledoc false

  # Dispatcher is the entry point for messages from JS, which it
  # routes to Local LiveViews. It receives two kinds of messages:
  # - Actions from LLV's JS part. They include requests to create a LLV
  #   and to dispatch data to LLVs directly (by sending a message to LLV's
  #   process)
  # - Phoenix transport frames. The dispatcher sits on the Phoenix.Socket.Transport
  #   level (public API; the same thing that LV's websocket and longpool implement).
  #   The frames are forwarded from JS to the Phoenix.LiveView.Socket and vice versa.

  use GenServer
  require Logger
  import Popcorn.Wasm

  alias __MODULE__.View
  alias Popcorn.Wasm
  alias Phoenix.Socket.{Message, Reply}

  @process_name :main

  @table :local_live_view_dispatcher

  @doc false
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  def current_url do
    case :ets.lookup(@table, :url) do
      [{:url, url}] -> url
      [] -> nil
    end
  end

  def current_assigns(id) do
    case :ets.lookup(@table, {:assigns, id}) do
      [{{:assigns, ^id}, assigns}] -> assigns
      [] -> nil
    end
  end

  def register_channel(id, epoch) do
    send(@process_name, {:llv_register_channel, id, epoch, self()})
    :ok
  end

  @impl true
  def init(_init_arg) do
    {:ok, transport} =
      Phoenix.LiveView.Socket.connect(%{
        endpoint: LocalLiveView.Endpoint,
        transport: :popcorn,
        options: [serializer: [{__MODULE__.Serializer, "~> 2.0"}]],
        params: %{"vsn" => "2.0.0"},
        connect_info: %{session: %{}}
      })

    {:ok, transport} = Phoenix.LiveView.Socket.init(transport)

    :ets.new(@table, [:named_table])

    Popcorn.Wasm.ready(@process_name)

    {:ok,
     %{
       transport: transport,
       views: %{}
     }}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    {:wasm_call, msg, promise} = Wasm.parse_message!(raw_msg)

    case handle_wasm_call(msg, promise, state) do
      {:resolve, reply, state} ->
        Wasm.resolve(reply, promise)
        {:noreply, state}

      {:reject, reason, state} ->
        Wasm.reject(reason, promise)
        {:noreply, state}

      {:ignore, state} ->
        {:noreply, state}
    end
  end

  # Reply from the LiveView.Socket
  def handle_info({:socket_push, _opcode, %Reply{topic: topic, ref: ref} = reply}, state) do
    id = topic_to_id(topic)

    case state.views[id] do
      nil ->
        {:noreply, state}

      view ->
        {promise, view} = View.pop_reply(view, ref)
        if promise, do: Wasm.resolve(ack(reply), promise)
        {:noreply, put_in(state.views[id], view)}
    end
  end

  # Message from the LiveView.Socket to the LiveView JS
  def handle_info({:socket_push, _opcode, %Message{} = message}, state) do
    {:noreply, route_browser_push(message, state)}
  end

  # Handle LLV process' death
  def handle_info({:DOWN, ref, :process, _pid, reason} = msg, state) do
    case Enum.find(state.views, fn {_id, view} -> view.monitor_ref == ref end) do
      {id, view} ->
        # TODO: drop the log once migrated to BEAM, where GenServer reports it
        unless reason in [:normal, :shutdown] or match?({:shutdown, _}, reason) do
          Logger.error("LLV #{id} channel exited: #{inspect(reason)}")
        end

        {:noreply, put_in(state.views[id], View.channel_down(view))}

      nil ->
        socket_handle_info(msg, state)
    end
  end

  # Handle LLV's registration via register_channel/2
  def handle_info({:llv_register_channel, id, epoch, pid}, state) do
    case state.views[id] do
      %View{epoch: ^epoch} ->
        monitor_ref = Process.monitor(pid)
        {:noreply, update_in(state.views[id], &View.register_channel(&1, pid, monitor_ref))}

      _other ->
        # The view was destroyed (and possibly re-created) while this
        # channel's join was in flight — too early for the destroy to stop
        # it (a pid is only known after registration), so stop it here.
        Logger.debug("LLV #{id}: registration from a dead incarnation ignored")
        Process.exit(pid, {:shutdown, :destroyed})
        {:noreply, state}
    end
  end

  # Anything else is treated as socket-bound: the dispatcher is the
  # transport process, so unrecognized messages belong to the socket <->
  # channel protocol — a surface Phoenix owns and may extend. The socket
  # interprets its own messages (truly unknown ones hit its {:ok, state}
  # catch-all), and socket_handle_info refuses any stop it answers with,
  # so no protocol message can take the dispatcher down.
  def handle_info(msg, state) do
    socket_handle_info(msg, state)
  end

  ## Browser/host actions

  # This event may be fired multiple times for the same view from JS,
  # in such case we only handle the first event.
  #
  # The session LocalLiveView.Proxy reads at mount (the key contract lives
  # in proxy.ex) is assembled here from the bare wire values, then signed
  # into the join token by render_container. That includes the incarnation
  # epoch, minted per create — a create for an id can only follow that
  # id's destroy (the guard rejects it otherwise), so a fresh ref per
  # create is a fresh ref per incarnation; it is read back by the proxy
  # and returned in register_channel to prove which incarnation a mounting
  # channel was created for.
  defp handle_wasm_call(
         %{
           "action" => "create",
           "id" => id,
           "view" => view,
           "mirror_id" => mirror_id,
           "assigns" => assigns
         },
         _promise,
         state
       )
       when not is_map_key(state.views, id) do
    # When an LLV is created, destroyed/crashes and created again
    # in a short period, there can be multiple LLV processes for
    # a single LLV at the same time. Thus, we add a reference
    # to make sure the LLV process registered with register/2
    # is the newest one.
    epoch = make_ref()

    # The LLV needs the assigns at mount, and we get to know
    # about its registration only after it mounts, thus we put
    # assigns into an ETS table that the LLV can read during mount.
    if assigns, do: :ets.insert(@table, {{:assigns, id}, assigns})
    state = put_in(state.views[id], %View{epoch: epoch})
    session = %{"llv" => %{id: id, view: view, epoch: epoch, mirror_id: mirror_id}}
    {:resolve, %{html: View.render_container(id, session)}, state}
  end

  defp handle_wasm_call(%{"action" => "create"}, _promise, state) do
    {:reject, "error creating LLV", state}
  end

  # Frame sent by the JS transport
  defp handle_wasm_call(
         %{"action" => "transport_frame", "id" => id, "frame" => frame},
         promise,
         state
       )
       when is_map_key(state.views, id) do
    case socket_in(state, frame) do
      {:pending, ref, state} ->
        # The channel replies asynchronously; hold the promise under the ref.
        {:ignore, update_in(state.views[id], &View.put_reply(&1, ref, promise))}

      {{:reply, reply}, _ref, state} ->
        {:resolve, ack(reply), state}
    end
  end

  # When phx_leave comes, we may have removed the view from the state already
  defp handle_wasm_call(
         %{"action" => "transport_frame", "frame" => %{"event" => "phx_leave"} = frame},
         _promise,
         state
       ) do
    {_result, _ref, state} =
      socket_in(state, frame)

    {:resolve, %{status: :ok, payload: %{}}, state}
  end

  defp handle_wasm_call(%{"action" => "transport_frame"}, _promise, state) do
    {:reject, "view not mounted", state}
  end

  # Keep the current url in the ETS table, so that LLVs can read it
  defp handle_wasm_call(%{"action" => "url_changed", "url" => url}, _promise, state) do
    :ets.insert(@table, {:url, url})
    {:resolve, :ok, state}
  end

  # A navigation the views did not initiate (standalone patch links and
  # popstate, or the host's phx:navigate): refresh the URL cache, then
  # re-run handle_params in every view with a live channel. Views without
  # one — still mounting, or awaiting a crash-rejoin — are skipped on
  # purpose, not queued: their (re)mount reads the fresh cache, so a
  # queued message would only replay a navigation the mount already saw.
  defp handle_wasm_call(%{"action" => "navigated", "url" => url}, _promise, state) do
    :ets.insert(@table, {:url, url})

    for {_id, %View{channel_pid: pid}} <- state.views, is_pid(pid) do
      send(pid, {:llv, %{"action" => "handle_params", "url" => url}})
    end

    {:resolve, :ok, state}
  end

  # The host re-rendered a view's data-pop-assigns. Assigns are STATE, not
  # events, so they take a latest-wins path instead of the pending queue:
  # the value goes into the ETS (opaque wire encoding — stored, never
  # parsed; only LocalLiveView.Proxy decodes it), where any later mount of
  # this incarnation reads it before its initial render, and is also sent
  # to the channel when one is live. An unknown id is a benign teardown
  # race.
  defp handle_wasm_call(
         %{"action" => "update_assigns", "id" => id, "assigns" => assigns},
         _promise,
         state
       ) do
    case state.views[id] do
      nil ->
        {:resolve, :ok, state}

      view ->
        :ets.insert(@table, {{:assigns, id}, assigns})

        if is_pid(view.channel_pid) do
          send(view.channel_pid, {:llv, %{"action" => "update_assigns", "assigns" => assigns}})
        end

        {:resolve, :ok, state}
    end
  end

  # Messages the dispatcher only relays: the payload is wrapped in {:llv, _}
  # and handed to View.dispatch — sent to the joined channel, or queued in
  # arrival order until one registers. LocalLiveView.Proxy parses the
  # payload's "action".
  #
  # With "queue" => "unless_dead" the caller opts into a verdict: a dead
  # view (crash → rejoin window, or a clean stop that never rejoins) and an
  # unknown id reject the promise instead of queueing — for messages
  # carrying caller intent (pushEvent) that must not silently replay
  # against a remounted view. Without it, unknown ids resolve ok (benign
  # teardown races) and everything queues.
  defp handle_wasm_call(
         %{"action" => "dispatch_to_view", "id" => id, "payload" => payload} = msg,
         _promise,
         state
       ) do
    unless_dead? = msg["queue"] == "unless_dead"

    case state.views[id] do
      nil when unless_dead? ->
        {:reject, "view not mounted", state}

      nil ->
        {:resolve, :ok, state}

      view ->
        opts = if unless_dead?, do: [queue: :unless_dead], else: []

        case View.dispatch(view, {:llv, payload}, opts) do
          {:ok, view} -> {:resolve, :ok, put_in(state.views[id], view)}
          :dead -> {:reject, "view is dead", state}
        end
    end
  end

  defp handle_wasm_call(%{"action" => "destroy", "id" => id}, _promise, state) do
    # The host LiveView removed a mount point: settle the entry's
    # outstanding promises (error acks instead of timeouts), forget it —
    # cached assigns included — and stop its channel the way a real
    # transport does when its client side is gone (channels die with their
    # transport; no frame is fabricated). The exit signal kills without
    # running terminate/2 — LiveView channels don't trap exits — the same
    # as any channel crash. A channel still mid-join has no pid here yet;
    # it is stopped at its own registration instead (see the
    # dead-incarnation branch). A leave the client sends later finds no
    # entry and no channel — the phx_leave fallback clause answers it.
    :ets.delete(@table, {:assigns, id})
    {view, views} = Map.pop(state.views, id)

    if view do
      View.channel_down(view)
      if is_pid(view.channel_pid), do: Process.exit(view.channel_pid, {:shutdown, :destroyed})
    end

    {:resolve, :ok, %{state | views: views}}
  end

  ## Socket plumbing

  # Feeds a channel frame through the socket layer, threading the transport
  # state. Refs are always the browser's real ones — the dispatcher
  # fabricates no frames — and the socket only delivers frames whose
  # join_ref matches the one it recorded at join.
  # Returns {:pending, ref, state} when the channel will reply
  # asynchronously (via the socket_push envelope), or
  # {{:reply, %Reply{}}, ref, state} when the socket settles the frame
  # itself — synchronously-replied joins included.
  defp socket_in(state, frame) do
    message = %Message{
      topic: frame["topic"],
      event: frame["event"],
      payload: frame["payload"],
      ref: frame["ref"],
      join_ref: frame["join_ref"]
    }

    case Phoenix.LiveView.Socket.handle_in({message, []}, state.transport) do
      {:ok, transport} ->
        {:pending, message.ref, %{state | transport: transport}}

      {:reply, _status, {_opcode, %Reply{} = reply}, transport} ->
        {{:reply, reply}, message.ref, %{state | transport: transport}}
    end
  end

  # The browser ack for a settled frame; the JS transport closure re-attaches
  # the original refs, so status and payload are all it needs.
  defp ack(%Reply{status: status, payload: payload}), do: %{status: status, payload: payload}

  defp socket_handle_info(msg, state) do
    case Phoenix.LiveView.Socket.handle_info(msg, state.transport) do
      {:ok, transport} ->
        {:noreply, %{state | transport: transport}}

      {:push, {_opcode, message}, transport} ->
        {:noreply, route_browser_push(message, %{state | transport: transport})}

      {:stop, reason, transport} ->
        Logger.warning("LLV dispatcher ignoring socket stop: #{inspect(reason)}")
        {:noreply, %{state | transport: transport}}
    end
  end

  defp route_browser_push(%Message{event: "redirect", topic: topic, payload: payload}, state) do
    # A local view called Phoenix's redirect/2 (the one navigation that works
    # without a router, so the push is real). A Wasm view cannot redirect the
    # browser, and the channel stops itself right after this push — a clean
    # shutdown, so no crash-rejoin: the view is left permanently dead. The
    # phx_close that follows is forwarded like any push, so the client
    # channel closes cleanly (LV's quiet clean-close handling, no rejoin)
    # instead of erroring on every later interaction.
    Logger.error("""
    LLV #{topic_to_id(topic)}: redirect/2 is not supported in local views — \
    redirect to #{inspect(payload[:to] || payload[:external])} ignored, \
    the LLV terminated.
    """)

    state
  end

  defp route_browser_push(%Message{event: "live_redirect", topic: topic, payload: payload}, state) do
    # push_navigate from handle_info/mount: the channel pushes live_redirect
    # BEFORE its sticky check, then (sticky) drops the redirect and keeps
    # running. Local views are not router-mounted, so there is nothing to
    # navigate — the view just continues.
    Logger.error("""
    LLV #{topic_to_id(topic)}: push_navigate is not supported in local views \
    — navigation to #{inspect(payload[:to])} ignored.
    """)

    state
  end

  defp route_browser_push(message, state) do
    # Everything else — diffs from handle_info/send_update/async results,
    # phx_error (the stock phoenix.js channel schedules a rejoin),
    # phx_close (a clean stop; the client channel closes, no rejoin),
    # whatever future LiveView clients listen for tomorrow — is forwarded
    # verbatim, carrying the pushing incarnation's join_ref so isMember
    # drops stale frames. The browser side injects it into the channel,
    # where stock phoenix.js dispatches by event name; events nothing
    # listens for are inert there.
    push_browser(message, state)
    state
  end

  defp topic_to_id("lv:" <> id), do: id

  # Forwarded whenever the id still has an entry (a destroyed view's late
  # pushes stop at this gate). Staleness beyond that is the browser's to
  # judge, with the frame's own refs: phoenix.js ignores topics no channel
  # claims and isMember drops join_refs of previous incarnations.
  defp push_browser(%Message{topic: topic} = message, state) do
    if Map.has_key?(state.views, topic_to_id(topic)) do
      Popcorn.Wasm.run_js(
        """
        ({ args }) => {
          window.__llvPopcornTransportPush?.(args);
        }
        """,
        Map.from_struct(message)
      )
    end

    :ok
  end
end
