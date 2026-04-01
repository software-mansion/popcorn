defmodule LocalLiveView.Phoenix do
  @moduledoc """
  Helpers for Phoenix LiveViews that host LocalLiveViews.

  Provides a `handle_local_sync/2` callback that replaces the boilerplate
  `handle_event("llv_local_message", ...)` pattern.

  ## Usage

      defmodule MyAppWeb.MyLive do
        use MyAppWeb, :live_view
        use LocalLiveView.Phoenix

        def handle_local_sync(%{"type" => "sync", "users" => users}, socket) do
          {:noreply, assign(socket, users: users)}
        end
      end

  """

  defmacro __using__(_opts) do
    quote do
      @behaviour LocalLiveView.Phoenix

      def handle_event("sync", payload, socket) do
        handle_local_sync(payload, socket)
      end
    end
  end

  @callback handle_local_sync(
              payload :: map(),
              socket :: Phoenix.LiveView.Socket.t()
            ) ::
              {:noreply, Phoenix.LiveView.Socket.t()}
              | {:reply, map(), Phoenix.LiveView.Socket.t()}
end
