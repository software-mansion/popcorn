defmodule LocalComponent do
  alias Phoenix.LiveView.Socket

  @doc false
  defmacro __using__(opts \\ []) do
    quote bind_quoted: [opts: opts] do
      import LocalComponent
    end
  end

  @doc type: :macro
  defmacro sigil_H({:<<>>, meta, [expr]}, modifiers)
           when modifiers == [] or modifiers == ~c"noformat" do
    if not Macro.Env.has_var?(__CALLER__, {:assigns, nil}) do
      raise "~H requires a variable named \"assigns\" to exist and be set to a map"
    end

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

  defp validate_assign_key!(key) when is_atom(key), do: :ok

  defp validate_assign_key!(key) do
    raise ArgumentError, "assigns in LiveView must be atoms, got: #{inspect(key)}"
  end

  def update(socket_or_assigns, key, fun)

  def update(%Socket{assigns: assigns} = socket, key, fun) when is_function(fun, 2) do
    update(socket, key, &fun.(&1, assigns))
  end

  def update(%Socket{assigns: assigns} = socket, key, fun) when is_function(fun, 1) do
    case assigns do
      %{^key => val} -> Phoenix.LiveView.Utils.assign(socket, key, fun.(val))
      %{} -> raise KeyError, key: key, term: assigns
    end
  end

  def update(assigns, key, fun) when is_function(fun, 2) do
    update(assigns, key, &fun.(&1, assigns))
  end

  def update(assigns, key, fun) when is_function(fun, 1) do
    case assigns do
      %{^key => val} -> assign(assigns, key, fun.(val))
      %{} -> raise KeyError, key: key, term: assigns
    end
  end

  def update(assigns, _key, fun) when is_function(fun, 1) or is_function(fun, 2) do
    raise_bad_socket_or_assign!("update/3", assigns)
  end
end
