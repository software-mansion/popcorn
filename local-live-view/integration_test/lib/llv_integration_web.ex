defmodule LlvIntegrationWeb do
  @moduledoc false

  def router do
    quote do
      use Phoenix.Router, helpers: false

      import Plug.Conn
      import Phoenix.Controller
      import Phoenix.LiveView.Router
      import LocalLiveView.Router
    end
  end

  def controller do
    quote do
      use Phoenix.Controller, formats: [:html]
      import Plug.Conn
    end
  end

  def live_view do
    quote do
      use Phoenix.LiveView
      unquote(html_helpers())
    end
  end

  def html do
    quote do
      use Phoenix.Component
      import Phoenix.Controller, only: [get_csrf_token: 0]
      unquote(html_helpers())
    end
  end

  defp html_helpers do
    quote do
      import Phoenix.HTML
      import LocalLiveView.Component
      alias Phoenix.LiveView.JS
    end
  end

  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
