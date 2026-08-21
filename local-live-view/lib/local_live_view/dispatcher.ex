defmodule LocalLiveView.Serializer do
  @moduledoc false
  # The dispatcher and the LiveView channel run in the same runtime: channel
  # traffic stays plain Elixir terms, so "encoding" is the identity.
  def encode!(message), do: message
end

defmodule LocalLiveView.Dispatcher do
  @moduledoc false

  # Routes browser/host traffic to per-view Phoenix.LiveView.Channel processes.
  #
  # It registers as popcorn's :main process to receive wasm messages. The
  # browser's "create" call stores a view's config; the channel join frame then
  # starts a REAL Phoenix.LiveView.Channel with this process posing as its
  # transport: the fake %Phoenix.Socket{} points transport_pid here (with a
  # passthrough serializer), the session token the channel verifies is signed
  # on the spot, browser frames are forwarded as %Phoenix.Socket.Message{}, and
  # each frame's popcorn.call promise is settled from the channel's
  # %Phoenix.Socket.Reply{}.

  use GenServer
  require Logger
  import Popcorn.Wasm
  alias Popcorn.Wasm
  alias Phoenix.Socket.{Message, Reply}

  @process_name :main

  # join_ref of the fake %Phoenix.Socket{}. The browser-side ack echoes the
  # original frame's refs (kept in a transport closure), so this never has to
  # match anything on the page.
  @join_ref "1"

  @doc false
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    Popcorn.Wasm.ready(@process_name)
    {:ok, %{views: %{}, joins: %{}, replies: %{}, counter: 0}}
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

  # A channel reply (Phoenix.Channel.reply through the passthrough serializer):
  # settle the promise of the browser frame the ref was minted for.
  def handle_info(%Reply{topic: topic, ref: ref} = reply, state) do
    {entry, replies} = Map.pop(state.replies, {topic, ref})

    case entry do
      {:promise, promise} ->
        Wasm.resolve(%{status: reply.status, payload: reply.payload}, promise)

      :oob ->
        # Ack of a dispatcher-initiated event (host server_message): there is
        # no browser push waiting, so a diff goes out-of-band like any
        # handle_info render.
        with %{diff: diff} <- reply.payload do
          push_browser_diff(topic_to_id(topic), diff)
        end

        if reply.status != :ok do
          Logger.error("LLV server message failed: #{inspect(reply.payload)}")
        end

      nil ->
        :ok
    end

    {:noreply, %{state | replies: replies}}
  end

  # A channel push: diffs produced outside a browser frame (handle_info,
  # send_update, async results).
  def handle_info(%Message{topic: topic, event: "diff", payload: diff}, state) do
    push_browser_diff(topic_to_id(topic), diff)
    {:noreply, state}
  end

  def handle_info(%Message{event: event}, state) do
    # live_patch/redirect/live_redirect pushes need router-mounted sessions,
    # which local views never have (LocalLiveView.push_patch runs through the
    # {:llv, :patch} hook instead).
    Logger.warning("LLV dispatcher ignoring channel push #{inspect(event)}")
    {:noreply, state}
  end

  # GenServer.reply of a channel join: the mount result.
  def handle_info({ref, join_result}, state) when is_map_key(state.joins, ref) do
    {%{id: id, promise: promise}, joins} = Map.pop(state.joins, ref)
    state = %{state | joins: joins}

    case join_result do
      {:ok, reply} ->
        Wasm.resolve(%{status: :ok, payload: reply}, promise)
        {:noreply, state}

      {:error, reason} ->
        # The channel already stopped itself after a failed mount.
        Wasm.resolve(%{status: :error, payload: reason}, promise)
        {:noreply, forget_channel(state, id)}
    end
  end

  def handle_info({:DOWN, monitor, :process, _pid, reason}, state) do
    case Enum.find(state.views, fn {_id, view} -> view.monitor == monitor end) do
      {id, _view} ->
        unless reason in [:normal, :shutdown] or match?({:shutdown, _}, reason) do
          Logger.error("LLV #{id} channel exited: #{inspect(reason)}")
        end

        {:noreply, forget_channel(state, id)}

      nil ->
        {:noreply, state}
    end
  end

  # Sent by the channel before a graceful stop; the :DOWN above cleans up.
  def handle_info({:socket_close, _pid, _reason}, state), do: {:noreply, state}
  def handle_info(:socket_drain, state), do: {:noreply, state}

  def handle_info(msg, state) do
    Logger.warning("LLV dispatcher ignoring message #{inspect(msg)}")
    {:noreply, state}
  end

  ## Browser/host actions

  # This event may be fired multiple times for the same view from JS,
  # in such case we only handle the first event.
  defp handle_wasm_call(%{"action" => "create", "id" => id} = msg, _promise, state)
       when not is_map_key(state.views, id) do
    view = String.to_atom("Elixir." <> Map.fetch!(msg, "view"))

    case load_live_view(view) do
      {:ok, _config} ->
        view_state = %{
          view: view,
          assigns: parse_assigns(msg["assigns"]),
          url: msg["url"],
          url_params: msg["url_params"] || %{},
          mirror_id: msg["mirror_id"],
          pid: nil,
          monitor: nil
        }

        {:resolve, :ok, put_in(state.views[id], view_state)}

      {:error, _reason} ->
        {:reject, "error creating LLV", state}
    end
  end

  defp handle_wasm_call(%{"action" => "create"}, _promise, state) do
    {:reject, "error creating LLV", state}
  end

  defp handle_wasm_call(
         %{"action" => "transport_frame", "id" => id, "event" => "phx_join"},
         promise,
         state
       )
       when is_map_key(state.views, id) do
    join_view(id, promise, state)
  end

  defp handle_wasm_call(%{"action" => "transport_frame", "id" => id} = msg, promise, state)
       when is_map_key(state.views, id) do
    case state.views[id] do
      %{pid: pid} when is_pid(pid) ->
        ref = "llv-" <> Integer.to_string(state.counter)

        send(pid, %Message{
          topic: topic(id),
          event: msg["event"],
          payload: msg["payload"],
          ref: ref,
          join_ref: @join_ref
        })

        {:ignore,
         %{
           state
           | counter: state.counter + 1,
             replies: Map.put(state.replies, {topic(id), ref}, {:promise, promise})
         }}

      _not_joined ->
        {:reject, "view not joined", state}
    end
  end

  defp handle_wasm_call(%{"action" => "transport_frame"}, _promise, state) do
    {:reject, "view not mounted", state}
  end

  defp handle_wasm_call(
         %{"action" => "update_assigns", "id" => id, "assigns" => assigns},
         _promise,
         state
       ) do
    case state.views[id] do
      %{pid: pid} when is_pid(pid) ->
        send(pid, {:llv, :update_assigns, parse_assigns(assigns)})
        {:resolve, :ok, state}

      %{} = view_state ->
        # Not joined yet: fold into the config the join will mount with.
        {:resolve, :ok, put_in(state.views[id], %{view_state | assigns: parse_assigns(assigns)})}

      nil ->
        {:resolve, :ok, state}
    end
  end

  defp handle_wasm_call(
         %{"action" => "handle_params", "id" => id, "payload" => %{"params" => params, "url" => url}},
         _promise,
         state
       ) do
    case state.views[id] do
      %{pid: pid} when is_pid(pid) ->
        send(pid, {:llv, :handle_params, params, url})
        {:resolve, :ok, state}

      %{} = view_state ->
        {:resolve, :ok, put_in(state.views[id], %{view_state | url: url, url_params: params})}

      nil ->
        {:resolve, :ok, state}
    end
  end

  defp handle_wasm_call(
         %{"action" => "push", "id" => id, "payload" => %{"event" => event, "payload" => payload}},
         _promise,
         state
       ) do
    send_to_view(state, id, {:js_push, event, payload})
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(
         %{"action" => "push_error", "id" => id, "payload" => %{"event" => event, "payload" => params}},
         _promise,
         state
       ) do
    send_to_view(state, id, {:llv, :push_error, event, params})
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(%{"action" => "reconnected", "id" => id}, _promise, state) do
    send_to_view(state, id, {:llv, :reconnected})
    {:resolve, :ok, state}
  end

  # Host-pushed llv_server_message: forwarded to the channel as a regular
  # "event" frame with a dispatcher-minted ref, so the view handles it in
  # handle_event like any other event; the reply is consumed above (:oob).
  defp handle_wasm_call(
         %{"action" => "server_message", "id" => id, "payload" => payload},
         _promise,
         state
       ) do
    case state.views[id] do
      %{pid: pid} when is_pid(pid) ->
        ref = "llv-oob-" <> Integer.to_string(state.counter)

        send(pid, %Message{
          topic: topic(id),
          event: "event",
          payload: payload,
          ref: ref,
          join_ref: @join_ref
        })

        {:resolve, :ok,
         %{
           state
           | counter: state.counter + 1,
             replies: Map.put(state.replies, {topic(id), ref}, :oob)
         }}

      _not_joined ->
        {:resolve, :ok, state}
    end
  end

  defp handle_wasm_call(%{"action" => "destroy", "id" => id}, _promise, state) do
    # The host LiveView removed a mount point. Stop the channel and forget it.
    state = if state.views[id], do: stop_channel(state, id), else: state
    {:resolve, :ok, %{state | views: Map.delete(state.views, id)}}
  end

  ## Join

  defp join_view(id, promise, state) do
    # A join for a live pid is a browser-channel rejoin after an error:
    # standard LiveView semantics remount from scratch.
    state = if state.views[id].pid, do: stop_channel(state, id), else: state
    view_state = state.views[id]

    token =
      Phoenix.LiveView.Static.sign_token(LocalLiveView.Endpoint, %{
        id: id,
        view: view_state.view,
        root_view: view_state.view,
        parent_pid: nil,
        root_pid: nil,
        session: %{
          "llv_id" => id,
          "assigns" => view_state.assigns,
          "url" => view_state.url,
          "url_params" => view_state.url_params,
          "mirror_id" => view_state.mirror_id
        },
        assign_new: []
      })

    phx_socket = %Phoenix.Socket{
      endpoint: LocalLiveView.Endpoint,
      topic: topic(id),
      transport_pid: self(),
      serializer: LocalLiveView.Serializer,
      join_ref: @join_ref,
      handler: Phoenix.LiveView.Socket,
      private: %{connect_info: %{session: %{}}}
    }

    # The browser's join payload carries no session (the fake view's channel is
    # created bare) — build the payload the channel expects from our config.
    join_payload = %{
      "session" => token,
      "url" => view_state.url,
      "params" => %{"_mounts" => 0}
    }

    ref = make_ref()

    {:ok, pid} =
      DynamicSupervisor.start_child(LocalLiveView.ChannelSupervisor, %{
        id: Phoenix.LiveView.Channel,
        start:
          {Phoenix.LiveView.Channel, :start_link, [{LocalLiveView.Endpoint, {self(), ref}}]},
        restart: :temporary
      })

    send(pid, {Phoenix.Channel, join_payload, {self(), ref}, phx_socket})

    view_state = %{view_state | pid: pid, monitor: Process.monitor(pid)}

    {:ignore,
     %{
       state
       | views: Map.put(state.views, id, view_state),
         joins: Map.put(state.joins, ref, %{id: id, promise: promise})
     }}
  end

  ## Helpers

  defp load_live_view(view) do
    # Force-load the module; a failure means the view name is wrong.
    {:ok, view.__live__()}
  rescue
    _ -> {:error, :stale}
  end

  defp topic(id), do: "lv:" <> id
  defp topic_to_id("lv:" <> id), do: id

  defp parse_assigns(nil), do: %{}
  defp parse_assigns(assigns), do: :erlang.binary_to_term(Base.decode64!(assigns))

  # Deliver to a view's channel, ignoring events for an id that isn't joined
  # (e.g. an event that arrives just after the view was torn down).
  defp send_to_view(state, id, message) do
    case state.views[id] do
      %{pid: pid} when is_pid(pid) -> send(pid, message)
      _ -> :ok
    end
  end

  defp stop_channel(state, id) do
    %{pid: pid, monitor: monitor} = state.views[id]

    if monitor, do: Process.demonitor(monitor, [:flush])
    if pid, do: DynamicSupervisor.terminate_child(LocalLiveView.ChannelSupervisor, pid)

    forget_channel(state, id)
  end

  # A channel is gone: settle its outstanding promises (so the browser gets an
  # error ack instead of a timeout) and keep only the config, which a later
  # browser-channel rejoin mounts a fresh process from.
  defp forget_channel(state, id) do
    topic = topic(id)

    {dead, kept} = Enum.split_with(state.replies, fn {{t, _ref}, _} -> t == topic end)

    for {_key, {:promise, promise}} <- dead do
      Wasm.resolve(%{status: :error, payload: %{reason: "view exited"}}, promise)
    end

    {dead_joins, kept_joins} = Enum.split_with(state.joins, fn {_ref, join} -> join.id == id end)

    for {_ref, %{promise: promise}} <- dead_joins do
      Wasm.resolve(%{status: :error, payload: %{reason: "view exited"}}, promise)
    end

    views =
      case state.views do
        %{^id => view} -> Map.put(state.views, id, %{view | pid: nil, monitor: nil})
        _ -> state.views
      end

    %{state | views: views, replies: Map.new(kept), joins: Map.new(kept_joins)}
  end

  defp push_browser_diff(_id, diff) when diff == %{}, do: :ok

  defp push_browser_diff(id, diff) do
    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        window.__popcornTransportReceive(args.id, args.diff);
      }
      """,
      %{id: id, diff: diff}
    )

    :ok
  end
end
