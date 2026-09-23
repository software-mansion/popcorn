defmodule LocalLiveView.SSR do
  @moduledoc false
  alias LocalLiveView.Lifecycle
  alias Phoenix.LiveView.{Diff, Rendered, Socket}

  @spec render(%{
          view: String.t(),
          assigns: map(),
          id: String.t() | nil,
          url: String.t() | nil,
          mirror_id: String.t() | nil
        }) :: {:ok, iodata()} | {:error, :not_loaded | :redirected}
  def render(%{view: view, assigns: assigns} = opts) when is_binary(view) and is_map(assigns) do
    module = Module.concat([view])

    with {:module, ^module} <- Code.ensure_loaded(module),
         true <- function_exported?(module, :render, 1) do
      do_render(module, opts)
    else
      _ -> {:error, :not_loaded}
    end
  end

  defp do_render(module, opts) do
    socket = build_socket(module, opts)

    {socket, _mount_opts} = Lifecycle.mount(module, :not_mounted_at_router, %{}, socket)

    socket =
      socket
      |> then(&Lifecycle.update!(module, opts.assigns, &1))
      |> then(&Lifecycle.handle_params(module, opts[:url], &1))

    if socket.redirected do
      {:error, :redirected}
    else
      rendered = to_rendered(socket, module)

      {diff, _prints, _components} =
        Diff.render(socket, rendered, Diff.new_fingerprints(), Diff.new_components())

      {:ok, Diff.to_iodata(diff)}
    end
  end

  # What Phoenix.LiveView.Renderer.to_rendered/2 does for a LiveView, minus
  # the layout: a LocalLiveView has none.
  defp to_rendered(socket, module) do
    assigns = socket.assigns
    socket = %{socket | assigns: %Socket.AssignsNotInSocket{__assigns__: assigns}}

    case module.render(Map.put(assigns, :socket, socket)) do
      %Rendered{} = rendered ->
        rendered

      other ->
        raise ArgumentError, """
        expected #{inspect(module)}.render/1 to return a %Phoenix.LiveView.Rendered{}, got:

        #{inspect(other)}
        """
    end
  end

  # Mirrors the socket the view gets in the browser (see LocalLiveView.Proxy),
  # minus the transport: no transport_pid makes connected?/1 false.
  defp build_socket(module, opts) do
    %Socket{
      id: opts[:id],
      endpoint: Application.get_env(:local_live_view, :default_endpoint),
      view: module,
      host_uri: host_uri(opts[:url]),
      assigns: %{__changed__: %{}, flash: %{}, live_action: nil},
      private: %{
        live_temp: %{},
        conn_session: %{},
        root_view: module,
        llv_view: module,
        llv_id: opts[:id],
        mirror_id: opts[:mirror_id]
      }
    }
  end

  defp host_uri(nil), do: :not_mounted_at_router

  defp host_uri(url) do
    case URI.parse(url) do
      %URI{host: host} = uri when is_binary(host) ->
        %URI{scheme: uri.scheme, host: host, port: uri.port}

      _relative ->
        :not_mounted_at_router
    end
  end
end
