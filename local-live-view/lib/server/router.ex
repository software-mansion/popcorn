defmodule LocalLiveView.Router do
  @moduledoc """
  Phoenix router macro for mounting a LocalLiveView at a route.

  Import this module in your Phoenix router:

      defmodule MyAppWeb.Router do
        use MyAppWeb, :router
        import LocalLiveView.Router

        scope "/" do
          pipe_through :browser
          live_local "/plain", HelloLocal
        end
      end
  """

  defmacro live_local(path, view_module) do
    view_string =
      case view_module do
        {:__aliases__, _, parts} -> parts |> List.last() |> to_string()
        name when is_binary(name) -> name
        name when is_atom(name) -> to_string(name)
      end

    quote do
      scope "/", alias: false do
        Phoenix.Router.get(
          unquote(path),
          LocalLiveView.Controller,
          :index,
          private: %{llv_view: unquote(view_string)}
        )
      end
    end
  end
end
