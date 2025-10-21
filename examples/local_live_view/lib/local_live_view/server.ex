defmodule LocalLiveView.Message do
  defstruct topic: nil, event: nil, payload: nil, ref: nil, join_ref: nil
end

defmodule LocalLiveView.Server do
  @moduledoc false
#  A LocalLiveView is a process that receives events, updates
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
    Upload,
    UploadConfig,
    Route,
    Session,
    Lifecycle,
    Async
  }

  alias Phoenix.LiveView.Socket
  alias Phoenix.LiveView.Diff

  alias Phoenix.Socket.Broadcast
  alias LocalLiveView.Message

  import Popcorn.Wasm
  alias Popcorn.Wasm

  @prefix :phoenix
  @not_mounted_at_router :not_mounted_at_router
  @max_host_size 253

  def start_llv_process() do
    DynamicSupervisor.start_child(
      LocalLiveView.Server.Supervisor,
      __MODULE__.child_spec([])
    )
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, [])
  end

  @impl true
  def init([]) do
    {:ok, []}
  end

  @impl true
  def handle_info({LocalLiveView.Server, params, from, phx_socket}, ref) do
    try do
      mount(params, from, phx_socket)
    rescue
      e ->
        reraise(e, __STACKTRACE__)
    end
  end

  def handle_info(%Message{event: "event"} = msg, state) do
    %{"value" => raw_val, "event" => event, "type" => type} = payload = msg.payload
    new_state = %{state | socket: maybe_update_uploads(state.socket, msg.payload)}
    val = %{}

    new_state.socket
    |> view_handle_event(event, val)
    |> handle_result({:handle_event, 3, msg.ref}, new_state)
  end

  def handle_info(msg, %{socket: socket} = state) do
    msg
    |> view_handle_info(socket)
    |> handle_result({:handle_info, 2, nil}, state)
  end

  defp view_handle_event(%Socket{} = socket, "lv:clear-flash", val) do
    case val do
      %{"key" => key} -> {:noreply, Utils.clear_flash(socket, key)}
      _ -> {:noreply, Utils.clear_flash(socket)}
    end
  end

  defp view_handle_event(%Socket{}, "lv:" <> _ = bad_event, _val) do
    raise ArgumentError, """
    received unknown LiveView event #{inspect(bad_event)}.
    The following LiveView events are supported: lv:clear-flash.
    """
  end

  defp view_handle_event(%Socket{} = socket, event, val) do
    case socket.view.handle_event(event, val, socket) do
      {:noreply, %Socket{} = socket} ->
        {:noreply, socket}

      {:reply, reply, %Socket{} = socket} ->
        {:reply, reply, socket}

      other ->
        raise_bad_callback_response!(other, socket.view, :handle_event, 3)
    end
  end

  defp view_handle_info(msg, %{view: view} = socket) do
    view.handle_info(msg, socket)
  end

  defp maybe_call_mount_handle_params(%{socket: socket} = state, router, url, params) do
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
        |> Utils.call_handle_params!(view, lifecycle.exported?, params, url)
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

  defp handle_result(
         {:reply, %{} = reply, %Socket{} = new_socket},
         {:handle_event, 3, ref},
         state
       ) do
    handle_changed(state, Utils.put_reply(new_socket, reply), ref)
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

      result ->
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
        rendered = Phoenix.LiveView.Renderer.to_rendered(socket, socket.view)

        rendered
        |> Phoenix.HTML.Safe.to_iodata()
        |> IO.iodata_to_binary()
        |> LocalLiveView.JS.rerender(socket.view)

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

  defp reply(state, {ref, extra}, status, payload) do
    reply(state, ref, status, Map.merge(payload, extra))
  end

  defp reply(state, ref, status, payload) when is_binary(ref) do
    state
  end

  defp push(state, event, payload) do
    view = state.socket.view
    state
  end

  #  ## Mount
  #
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

    connect_params = params["params"]

    socket = %Socket{
      view: view
    }

    lifecycle = load_lifecycle(config, nil)

    case mount_private(verified, connect_params, nil, lifecycle) do
      {:ok, mount_priv} ->
        socket = Utils.configure_socket(socket, mount_priv, nil, nil, :not_mounted_at_router)

        try do
          %Socket{socket | view: view}
          |> Utils.maybe_call_live_view_mount!(view, params, verified)
          |> build_state(phx_socket)
          |> maybe_call_mount_handle_params(nil, nil, params)
          |> reply_mount(from, verified, nil)
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

  defp reply_mount(result, from, %Session{} = session, route) do
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

  defp build_state(%Socket{} = lv_socket, %Phoenix.Socket{} = phx_socket) do
    %{
      join_ref: phx_socket.join_ref,
      serializer: phx_socket.serializer,
      socket: lv_socket,
      topic: phx_socket.topic,
      components: Diff.new_components(),
      fingerprints: Diff.new_fingerprints(),
      redirect_count: 0,
      upload_names: %{},
      upload_pids: %{}
    }
  end

  defp post_verified_mount(%{socket: socket} = state) do
    %{state | socket: Utils.post_mount_prune(socket)}
  end

  defp assign_action(socket, action) do
    Phoenix.LiveView.Utils.assign(socket, :live_action, action)
  end

  defp maybe_update_uploads(%Socket{} = socket, %{} = _payload), do: socket
end
