defprotocol Phoenix.HTML.FormData do
  @moduledoc """
  Converts a data structure into a `Phoenix.HTML.Form` struct.

  ## Ecto integration

  Phoenix provides integration of forms with Ecto changesets and data
  structures via the [phoenix_ecto](https://hex.pm/packages/phoenix_ecto) package.
  If a project was generated without Ecto support that dependency will need to be
  manually added.
  """

  @doc """
  Converts a data structure into a [`Phoenix.HTML.Form`](`t:Phoenix.HTML.Form.t/0`) struct.

  The options have their meaning defined by the underlying
  implementation but all shared options below are expected to
  be implemented. All remaining options must be stored in the
  returned struct.

  ## Shared options

    * `:as` - the value to be used as the form name

    * `:id` - the ID of the form attribute. All form inputs will
      be prefixed by the given ID

  """
  @spec to_form(t, Keyword.t()) :: Phoenix.HTML.Form.t()
  def to_form(data, options)

  @doc """
  Converts the field in the given form based on the data structure
  into a list of [`Phoenix.HTML.Form`](`t:Phoenix.HTML.Form.t/0`) structs.

  The options have their meaning defined by the underlying
  implementation but all shared options below are expected to
  be implemented. All remaining options must be stored in the
  returned struct.

  ## Shared Options

    * `:id` - the id to be used in the form, defaults to the
      concatenation of the given `field` to the parent form id

    * `:as` - the name to be used in the form, defaults to the
      concatenation of the given `field` to the parent form name

    * `:default` - the value to use if none is available

    * `:prepend` - the values to prepend when rendering. This only
      applies if the field value is a list and no parameters were
      sent through the form.

    * `:append` - the values to append when rendering. This only
      applies if the field value is a list and no parameters were
      sent through the form.

    * `:action` - The user defined action being taken by the form, such
      as `:validate`, `:save`, etc.
  """
  @spec to_form(t, Phoenix.HTML.Form.t(), Phoenix.HTML.Form.field(), Keyword.t()) ::
          [Phoenix.HTML.Form.t()]
  def to_form(data, form, field, options)

  @doc """
  Returns the value for the given field.
  """
  @spec input_value(t, Phoenix.HTML.Form.t(), Phoenix.HTML.Form.field()) :: term
  def input_value(data, form, field)

  @doc """
  Returns the HTML5 validations that would apply to
  the given field.
  """
  @spec input_validations(t, Phoenix.HTML.Form.t(), Phoenix.HTML.Form.field()) :: Keyword.t()
  def input_validations(data, form, field)
end

defimpl Phoenix.HTML.FormData, for: Map do
  def to_form(map, opts) do
    {name, params, opts} = name_params_and_opts(map, opts)
    {errors, opts} = Keyword.pop(opts, :errors, [])
    {action, opts} = Keyword.pop(opts, :action, nil)
    id = Keyword.get(opts, :id) || name

    unless is_binary(id) or is_nil(id) do
      raise ArgumentError, ":id option in form_for must be a binary/string, got: #{inspect(id)}"
    end

    %Phoenix.HTML.Form{
      source: map,
      impl: __MODULE__,
      id: id,
      name: name,
      params: params,
      data: %{},
      errors: errors,
      action: action,
      options: opts
    }
  end

  defp name_params_and_opts(map, opts) do
    with {key, _, _} when is_atom(key) <- :maps.next(:maps.iterator(map)) do
      IO.warn(
        "a map with atom keys was given to a form. Maps are always considered " <>
          "parameters and therefore must have string keys, got: #{inspect(map)}"
      )
    end

    case Keyword.pop(opts, :as) do
      {nil, opts} -> {nil, map, opts}
      {name, opts} -> {to_string(name), map, opts}
    end
  end

  def to_form(map, form, field, opts) when is_atom(field) or is_binary(field) do
    {default, opts} = Keyword.pop(opts, :default, %{})
    {prepend, opts} = Keyword.pop(opts, :prepend, [])
    {append, opts} = Keyword.pop(opts, :append, [])
    {name, opts} = Keyword.pop(opts, :as)
    {id, opts} = Keyword.pop(opts, :id)
    {hidden, opts} = Keyword.pop(opts, :hidden, [])
    {action, opts} = Keyword.pop(opts, :action, form.action)

    id = to_string(id || form.id <> "_#{field}")
    name = to_string(name || form.name <> "[#{field}]")
    params = Map.get(form.params, field_to_string(field))

    cond do
      # cardinality: one
      is_map(default) ->
        [
          %Phoenix.HTML.Form{
            source: map,
            impl: __MODULE__,
            id: id,
            name: name,
            data: default,
            action: action,
            params: params || %{},
            hidden: hidden,
            options: opts
          }
        ]

      # cardinality: many
      is_list(default) ->
        entries =
          if params do
            params
            |> Enum.sort_by(&elem(&1, 0))
            |> Enum.map(&{nil, elem(&1, 1)})
          else
            Enum.map(prepend ++ default ++ append, &{&1, %{}})
          end

        for {{data, params}, index} <- Enum.with_index(entries) do
          index_string = Integer.to_string(index)

          %Phoenix.HTML.Form{
            source: map,
            impl: __MODULE__,
            index: index,
            action: action,
            id: id <> "_" <> index_string,
            name: name <> "[" <> index_string <> "]",
            data: data,
            params: params,
            hidden: hidden,
            options: opts
          }
        end
    end
  end

  def input_value(_map, %{data: data, params: params}, field)
      when is_atom(field) or is_binary(field) do
    key = field_to_string(field)

    case params do
      %{^key => value} -> value
      %{} -> Map.get(data, field)
    end
  end

  def input_validations(_map, _form, _field), do: []

  # Normalize field name to string version
  defp field_to_string(field) when is_atom(field), do: Atom.to_string(field)
  defp field_to_string(field) when is_binary(field), do: field
end
