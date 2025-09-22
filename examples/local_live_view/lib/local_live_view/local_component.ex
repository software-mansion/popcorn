defmodule LocalComponent do

#  alias Phoenix.LiveView.{Static, Socket, AsyncResult}
  alias Phoenix.LiveView.Socket
  
  @doc false
  defmacro __using__(opts \\ []) do
#    conditional =
#      if __CALLER__.module != Phoenix.LiveView.Helpers do
#        quote do: import(Phoenix.LiveView.Helpers)
#      end

    imports =
      quote bind_quoted: [opts: opts] do
#        import Kernel, except: [def: 2, defp: 2]
#        import Phoenix.Component
#        import Phoenix.Component.Declarative
#        require Phoenix.Template
#
#        for {prefix_match, value} <- Phoenix.Component.Declarative.__setup__(__MODULE__, opts) do
#          @doc false
#          def __global__?(unquote(prefix_match)), do: unquote(value)
#        end
        import LocalComponent
      end

    [imports]
  end
  
  @doc type: :macro
  defmacro sigil_H({:<<>>, meta, [expr]}, modifiers)
           when modifiers == [] or modifiers == ~c"noformat" do
    if not Macro.Env.has_var?(__CALLER__, {:assigns, nil}) do
      raise "~H requires a variable named \"assigns\" to exist and be set to a map"
    end

#    TODO the following is a proper way of preparing a %Phoenix.LiveView.Rendered{} struct but for now LocalComponent will return a string
    options = [
      engine: Phoenix.LiveView.TagEngine,
      file: __CALLER__.file,
      line: __CALLER__.line + 1,
      caller: __CALLER__,
      indentation: meta[:indentation] || 0,
      source: expr,
      tag_handler: Phoenix.LiveView.HTMLEngine
    ]

    EEx.compile_string(expr, options)
  end


  @doc ~S'''
  Assigns the given `key` with value from `fun` into `socket_or_assigns` if one does not yet exist.

  The first argument is either a LiveView `socket` or an `assigns` map from function components.

  This function is useful for lazily assigning values and sharing assigns.
  We will cover both use cases next.

  ## Lazy assigns

  Imagine you have a function component that accepts a color:

  ```heex
  <.my_component bg_color="red" />
  ```

  The color is also optional, so you can skip it:

  ```heex
  <.my_component />
  ```

  In such cases, the implementation can use `assign_new` to lazily
  assign a color if none is given. Let's make it so it picks a random one
  when none is given:

      def my_component(assigns) do
        assigns = assign_new(assigns, :bg_color, fn -> Enum.random(~w(bg-red-200 bg-green-200 bg-blue-200)) end)

        ~H"""
        <div class={@bg_color}>
          Example
        </div>
        """
      end

  ## Sharing assigns

  It is possible to share assigns between the Plug pipeline and LiveView on disconnected render
  and between parent-child LiveViews when connected.

  ### When disconnected

  When a user first accesses an application using LiveView, the LiveView is first rendered in its
  disconnected state, as part of a regular HTML response. By using `assign_new` in the mount
  callback of your LiveView, you can instruct LiveView to re-use any assigns already set in `conn`
  during disconnected state.

  Imagine you have a Plug that does:

      # A plug
      def authenticate(conn, _opts) do
        if user_id = get_session(conn, :user_id) do
          assign(conn, :current_user, Accounts.get_user!(user_id))
        else
          send_resp(conn, :forbidden)
        end
      end

  You can re-use the `:current_user` assign in your LiveView during the initial render:

      def mount(_params, %{"user_id" => user_id}, socket) do
        {:ok, assign_new(socket, :current_user, fn -> Accounts.get_user!(user_id) end)}
      end

  In such case `conn.assigns.current_user` will be used if present. If there is no such
  `:current_user` assign or the LiveView was mounted as part of the live navigation, where no Plug
  pipelines are invoked, then the anonymous function is invoked to execute the query instead.

  ### When connected

  LiveView is also able to share assigns via `assign_new` with children LiveViews,
  as long as the child LiveView is also mounted when the parent LiveView is mounted
  and the child LiveView is not rendered with `sticky: true`. Let's see an example.

  If the parent LiveView defines a `:current_user` assign and the child LiveView also
  uses `assign_new/3` to fetch the `:current_user` in its `mount/3` callback, as in
  the previous subsection, the assign will be fetched from the parent LiveView, once
  again avoiding additional database queries.

  Note that `fun` also provides access to the previously assigned values:

      assigns =
        assigns
        |> assign_new(:foo, fn -> "foo" end)
        |> assign_new(:bar, fn %{foo: foo} -> foo <> "bar" end)

  Assigns sharing is performed when possible but not guaranteed. Therefore, you must
  ensure the result of the function given to `assign_new/3` is the same as if the value
  was fetched from the parent. Otherwise consider passing values to the child LiveView
  as part of its session.
  '''
  def assign_new(socket_or_assigns, key, fun)

  def assign_new(%Socket{} = socket, key, fun) do
    validate_assign_key!(key)
    Phoenix.LiveView.Utils.assign_new(socket, key, fun)
  end

  def assign_new(%{__changed__: changed} = assigns, key, fun) when is_function(fun, 1) do
    case assigns do
      %{^key => _} -> assigns
      %{} -> Phoenix.LiveView.Utils.force_assign(assigns, changed, key, fun.(assigns))
    end
  end

  def assign_new(%{__changed__: changed} = assigns, key, fun) when is_function(fun, 0) do
    case assigns do
      %{^key => _} -> assigns
      %{} -> Phoenix.LiveView.Utils.force_assign(assigns, changed, key, fun.())
    end
  end

  def assign_new(assigns, _key, fun) when is_function(fun, 0) or is_function(fun, 1) do
    raise_bad_socket_or_assign!("assign_new/3", assigns)
  end

  defp raise_bad_socket_or_assign!(name, assigns) do
    extra =
      case assigns do
        %_{} ->
          ""

        %{} ->
          """
          You passed an assigns map that does not have the relevant change tracking \
          information. This typically means you are calling a function component by \
          hand instead of using the HEEx template syntax. If you are using HEEx, make \
          sure you are calling a component using:

              <.component attribute={value} />

          If you are outside of HEEx and you want to test a component, use \
          Phoenix.LiveViewTest.render_component/2:

              Phoenix.LiveViewTest.render_component(&component/1, attribute: "value")

          """

        _ ->
          ""
      end

    raise ArgumentError,
          "#{name} expects a socket from Phoenix.LiveView/Phoenix.LiveComponent " <>
          " or an assigns map from Phoenix.Component as first argument, got: " <>
          inspect(assigns) <> extra
  end

  @doc """
  Adds a `key`-`value` pair to `socket_or_assigns`.

  The first argument is either a LiveView `socket` or an `assigns` map from function components.

  ## Examples

      iex> assign(socket, :name, "Elixir")

  """
  def assign(socket_or_assigns, key, value)

  def assign(%Socket{} = socket, key, value) do
    validate_assign_key!(key)
    Phoenix.LiveView.Utils.assign(socket, key, value)
  end

  def assign(%{__changed__: changed} = assigns, key, value) do
    case assigns do
      # force assign the key if the attribute was given with matching value
      %{^key => ^value, __given__: given} when not is_map_key(given, key) ->
        Phoenix.LiveView.Utils.force_assign(assigns, changed, key, value)

      %{^key => ^value} ->
        assigns

      %{} ->
        Phoenix.LiveView.Utils.force_assign(assigns, changed, key, value)
    end
  end

  def assign(assigns, _key, _val) do
    raise_bad_socket_or_assign!("assign/3", assigns)
  end

  @doc """
  Adds key-value pairs to assigns.

  The first argument is either a LiveView `socket` or an `assigns` map from function components.

  A keyword list or a map of assigns must be given as argument to be merged into existing assigns.

  ## Examples

      iex> assign(socket, name: "Elixir", logo: "💧")
      iex> assign(socket, %{name: "Elixir"})

  """
  def assign(socket_or_assigns, keyword_or_map)
      when is_map(keyword_or_map) or is_list(keyword_or_map) do
    Enum.reduce(keyword_or_map, socket_or_assigns, fn {key, value}, acc ->
      assign(acc, key, value)
    end)
  end

#  defp validate_assign_key!(:flash) do
#    raise ArgumentError,
#          ":flash is a reserved assign by LiveView and it cannot be set directly. " <>
#          "Use the appropriate flash functions instead"
#  end
#
#  defp validate_assign_key!(assign) when assign in @non_assignables do
#    raise ArgumentError,
#          "#{inspect(assign)} is a reserved assign by LiveView and it cannot be set directly"
#  end

  defp validate_assign_key!(key) when is_atom(key), do: :ok

  defp validate_assign_key!(key) do
    raise ArgumentError, "assigns in LiveView must be atoms, got: #{inspect(key)}"
  end

end
