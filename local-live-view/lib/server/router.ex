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

  @doc """
  Mounts a local live view at `path`, on a plain page with no host LiveView.

  Use it when the view needs no assigns from the server — the route renders
  nothing but the mount point, as if the page contained a single
  `LocalLiveView.Component.local_live_view/1` call. A view that receives
  assigns from the server, pushes events to it with
  `LocalLiveView.push_server_event/3` or handles
  `c:LocalLiveView.handle_push_error/4` needs a host LiveView instead, so
  declare it with `live/4` and render the component in its template.

  `view_module` is taken by its last alias segment, so `HelloLocal` and
  `MyApp.HelloLocal` both resolve to the `"HelloLocal"` view in the `local/`
  project. A string or atom works too.
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
