defmodule LocalLiveView.Lifecycle do
  @moduledoc false
  # Runs a view's callbacks, validating what they return. The browser
  # (`LocalLiveView.Proxy`) and the server (`LocalLiveView.SSR`) both go
  # through here, so a view mounts the same way in both places.

  alias Phoenix.LiveView.Socket

  @doc """
  Resolves a view name to its module, raising when it is not a LocalLiveView.
  """
  def resolve_view_module!(name) do
    module = Module.concat([name])
    loaded? = match?({:module, _module}, Code.ensure_loaded(module))

    if not loaded? or not function_exported?(module, :render, 1) do
      raise ArgumentError,
            "#{inspect(module)} (view #{inspect(name)}) is not a LocalLiveView — " <>
              "no such module, or it does not export render/1"
    end

    module
  end

  @doc """
  Calls the view's `mount/3`, if defined. Returns the socket and the mount options.
  """
  def mount(view, params, session, socket) do
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

  @doc """
  Calls the view's `update/2` with the assigns from the host.
  """
  def update!(view, assigns, socket) do
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

  @doc """
  Calls the view's `handle_params/3`, if defined, with the query params of `url`.
  """
  def handle_params(view, url, socket) do
    if function_exported?(view, :handle_params, 3) do
      case view.handle_params(query_params(url), url, socket) do
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

  # Query params are always derived from the URL they accompany — the same
  # parse for mount (create- or join-time URL) and live patches.
  defp query_params(nil), do: %{}

  defp query_params(url) do
    case String.split(url, "?", parts: 2) do
      [_path, query] -> URI.decode_query(query)
      [_path] -> %{}
    end
  end
end
