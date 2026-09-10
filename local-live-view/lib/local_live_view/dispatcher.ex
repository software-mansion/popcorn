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
  require Popcorn.Wasm, as: Wasm

  alias __MODULE__.View

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
  def handle_info(raw_msg, state) when Wasm.is_wasm_message(raw_msg) do
    {:wasm_call, msg, promise} = Wasm.parse_message!(raw_msg)

    case handle_wasm_call(msg, promise, state) do
      {:resolve, reply, state} ->
        Wasm.resolve(reply, promise)
        {:noreply, state}

      {:reject, reason, state} ->
        Wasm.reject(reason, promise)
        {:noreply, state}
    end
  end

  # Message/reply from the LiveView.Socket to the LiveView JS
  def handle_info({:socket_push, _opcode, message}, state) do
    push_to_browser(message, state)
    {:noreply, state}
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

  def handle_info(msg, state) do
    socket_handle_info(msg, state)
  end

  defp handle_wasm_call(
         %{"action" => "create", "id" => id} = msg,
         _promise,
         state
       )
       when not is_map_key(state.views, id) do
    %{"view" => view, "mirror_id" => mirror_id, "assigns" => assigns} = msg

    # When an LLV is created, destroyed/crashes and created again
    # in a short period, there can be multiple LLV processes for
    # a single LLV at the same time. Thus, we add a reference
    # to make sure the LLV process registered with register/2
    # is the newest one.
    epoch = make_ref()

    # The LLV needs the assigns at mount, and we get to know
    # about its registration only after it mounts, thus we put
    # assigns into an ETS table upfront, so the LLV can read
    # them during mount.
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
         _promise,
         state
       )
       when is_map_key(state.views, id) do
    case socket_in(state, frame) do
      {:pending, state} ->
        {:resolve, :ok, state}

      {{:reply, message}, state} ->
        push_to_browser(message, state)
        {:resolve, :ok, state}
    end
  end

  # When phx_leave comes, we may have removed the view from the state already
  defp handle_wasm_call(
         %{"action" => "transport_frame", "frame" => %{"event" => "phx_leave"} = frame},
         _promise,
         state
       ) do
    {_result, state} = socket_in(state, frame)
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(%{"action" => "transport_frame"}, _promise, state) do
    {:reject, "view not mounted", state}
  end

  defp handle_wasm_call(
         %{"action" => "update_assigns", "id" => id, "assigns" => assigns},
         _promise,
         state
       ) do
    if view = state.views[id] do
      :ets.insert(@table, {{:assigns, id}, assigns})
      View.dispatch(view, {:llv, %{"action" => "update_assigns", "assigns" => assigns}})
    end

    {:resolve, :ok, state}
  end

  # LLV-specific messages to be delivered to the view. See View.dispatch/2
  defp handle_wasm_call(
         %{"action" => "dispatch_to_view", "id" => id, "payload" => payload},
         _promise,
         state
       ) do
    case state.views[id] do
      nil ->
        Logger.debug("LLV #{id}: dispatch to a view that is not mounted ignored")
        {:resolve, :ok, state}

      view ->
        state = put_in(state.views[id], View.dispatch_queue(view, {:llv, payload}))
        {:resolve, :ok, state}
    end
  end

  defp handle_wasm_call(%{"action" => "destroy", "id" => id}, _promise, state) do
    :ets.delete(@table, {:assigns, id})
    {view, views} = Map.pop(state.views, id)

    if pid = get_in(view.channel_pid) do
      # We need to stop the view manually because it's sticky.
      # Thus, after a navigation, LiveView may not destroy the view
      # even if its DOM element is destroyed (that's what triggers
      # this action).
      Process.exit(pid, {:shutdown, :destroyed})
    end

    {:resolve, :ok, %{state | views: views}}
  end

  # Keep the current url in the ETS table, so that LLVs can read it
  defp handle_wasm_call(%{"action" => "url_changed", "url" => url}, _promise, state) do
    :ets.insert(@table, {:url, url})
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(%{"action" => "navigated", "url" => url}, _promise, state) do
    :ets.insert(@table, {:url, url})

    for {_id, view} <- state.views do
      View.dispatch(view, {:llv, %{"action" => "handle_params", "url" => url}})
    end

    {:resolve, :ok, state}
  end

  ## Socket plumbing

  defp socket_in(state, frame) do
    case Phoenix.LiveView.Socket.handle_in({frame, []}, state.transport) do
      {:ok, transport} ->
        {:pending, %{state | transport: transport}}

      {:reply, _status, {_opcode, reply_message}, transport} ->
        {{:reply, reply_message}, %{state | transport: transport}}
    end
  end

  defp socket_handle_info(msg, state) do
    case Phoenix.LiveView.Socket.handle_info(msg, state.transport) do
      {:ok, transport} ->
        {:noreply, %{state | transport: transport}}

      {:push, {_opcode, message}, transport} ->
        state = %{state | transport: transport}
        push_to_browser(message, state)
        {:noreply, state}

      {:stop, reason, transport} ->
        Logger.warning("LLV dispatcher ignoring socket stop: #{inspect(reason)}")
        {:noreply, %{state | transport: transport}}
    end
  end

  defp push_to_browser(%{topic: topic} = message, state) do
    case topic_to_id(topic) do
      nil ->
        Logger.warning("LLV dispatcher: dropping push on unsupported topic #{inspect(topic)}")

      id ->
        if validate_push(message) and Map.has_key?(state.views, id) do
          Popcorn.Wasm.run_js(
            ~S|({ args }) => { window.__llvPopcornTransportPush?.(args); }|,
            message
          )
        end
    end

    :ok
  end

  defp validate_push(%{event: "live_redirect", topic: topic, payload: payload}) do
    Logger.error("""
    LLV #{topic_to_id(topic)}: push_navigate is not supported in local views \
    — navigation to #{inspect(payload[:to])} ignored.
    """)

    false
  end

  defp validate_push(_message) do
    true
  end

  defp topic_to_id("lv:" <> id), do: id
  defp topic_to_id(_topic), do: nil
end
