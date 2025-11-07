defmodule Phoenix.HTML.Form do
  @moduledoc ~S"""
  Define a `Phoenix.HTML.Form` struct and functions to interact with it.

  For building actual forms in your Phoenix application, see
  [the `Phoenix.Component.form/1` component](https://hexdocs.pm/phoenix_live_view/Phoenix.Component.html#form/1).

  ## Access behaviour

  The `Phoenix.HTML.Form` struct implements the `Access` behaviour.
  When you do `form[field]`, it returns a `Phoenix.HTML.FormField`
  struct with the `id`, `name`, `value`, and `errors` prefilled.

  The field name can be either an atom or a string. If it is an atom,
  it assumes the form keeps both data and errors as atoms. If it is a
  string, it considers that data and errors are stored as strings for said
  field. Forms backed by an `Ecto.Changeset` only support atom field names.

  It is possible to "access" fields which do not exist in the source data
  structure. A `Phoenix.HTML.FormField` struct will be dynamically created
  with some attributes such as `name` and `id` populated.

  ## Custom implementations

  There is a protocol named `Phoenix.HTML.FormData` which can be implemented
  by any data structure that wants to be cast to the `Phoenix.HTML.Form` struct.
  """

  alias Phoenix.HTML.Form
  import Phoenix.HTML

  @doc """
  Defines the Phoenix.HTML.Form struct.

  Its fields are:

    * `:source` - the data structure that implements the form data protocol

    * `:action` - The action that was taken against the form. This value can be
      used to distinguish between different operations such as the user typing
      into a form for validation, or submitting a form for a database insert.

    * `:impl` - the module with the form data protocol implementation.
      This is used to avoid multiple protocol dispatches.

    * `:id` - the id to be used when generating input fields

    * `:index` - the index of the struct in the form

    * `:name` - the name to be used when generating input fields

    * `:data` - the field used to store lookup data

    * `:params` - the parameters associated with this form

    * `:hidden` - a keyword list of fields that are required to
      submit the form behind the scenes as hidden inputs

    * `:options` - a copy of the options given when creating the
      form without any form data specific key

    * `:errors` - a keyword list of errors that are associated with
      the form
  """
  defstruct source: nil,
            impl: nil,
            id: nil,
            name: nil,
            data: nil,
            action: nil,
            hidden: [],
            params: %{},
            errors: [],
            options: [],
            index: nil

  @type t :: %Form{
          source: Phoenix.HTML.FormData.t(),
          name: String.t(),
          data: %{field => term},
          action: atom(),
          params: %{binary => term},
          hidden: Keyword.t(),
          options: Keyword.t(),
          errors: [{field, term}],
          impl: module,
          id: String.t(),
          index: nil | non_neg_integer
        }

  @type field :: atom | String.t()

  @doc false
  def fetch(%Form{} = form, field) when is_atom(field) do
    fetch(form, field, Atom.to_string(field))
  end

  def fetch(%Form{} = form, field) when is_binary(field) do
    fetch(form, field, field)
  end

  def fetch(%Form{}, field) do
    raise ArgumentError,
          "accessing a form with form[field] requires the field to be an atom or a string, got: #{inspect(field)}"
  end

  defp fetch(%{errors: errors} = form, field, field_as_string) do
    {:ok,
     %Phoenix.HTML.FormField{
       errors: field_errors(errors, field),
       field: field,
       form: form,
       id: input_id(form, field_as_string),
       name: input_name(form, field_as_string),
       value: input_value(form, field)
     }}
  end

  @doc """
  Returns a value of a corresponding form field.

  The `form` should either be a `Phoenix.HTML.Form` or an atom.
  The field is either a string or an atom. If the field is given
  as an atom, it will attempt to look data with atom keys. If
  a string, it will look data with string keys.

  When a form is given, it will look for changes, then
  fallback to parameters, and finally fallback to the default
  struct/map value.

  Since the function looks up parameter values too, there is
  no guarantee that the value will have a certain type. For
  example, a boolean field will be sent as "false" as a
  parameter, and this function will return it as is. If you
  need to normalize the result of `input_value`, see
  `normalize_value/2`.
  """
  @spec input_value(t | atom, field) :: term
  def input_value(%{source: source, impl: impl} = form, field)
      when is_atom(field) or is_binary(field) do
    impl.input_value(source, form, field)
  end

  def input_value(name, _field) when is_atom(name), do: nil

  @doc """
  Returns an id of a corresponding form field.

  The form should either be a `Phoenix.HTML.Form` or an atom.
  """
  @spec input_id(t | atom, field) :: String.t()
  def input_id(%{id: nil}, field), do: "#{field}"

  def input_id(%{id: id}, field) when is_atom(field) or is_binary(field) do
    "#{id}_#{field}"
  end

  def input_id(name, field) when (is_atom(name) and is_atom(field)) or is_binary(field) do
    "#{name}_#{field}"
  end

  @doc """
  Returns an id of a corresponding form field and value attached to it.

  Useful for radio buttons and inputs like multiselect checkboxes.
  """
  @spec input_id(t | atom, field, Phoenix.HTML.Safe.t()) :: String.t()
  def input_id(name, field, value) do
    {:safe, value} = html_escape(value)
    value_id = value |> IO.iodata_to_binary() |> String.replace(~r/\W/u, "_")
    input_id(name, field) <> "_" <> value_id
  end

  @doc """
  Returns a name of a corresponding form field.

  The first argument should either be a `Phoenix.HTML.Form` or an atom.

  ## Examples

      iex> Phoenix.HTML.Form.input_name(:user, :first_name)
      "user[first_name]"
  """
  @spec input_name(t | atom, field) :: String.t()
  def input_name(form_or_name, field)

  def input_name(%{name: nil}, field), do: to_string(field)

  def input_name(%{name: name}, field) when is_atom(field) or is_binary(field),
    do: "#{name}[#{field}]"

  def input_name(name, field) when (is_atom(name) and is_atom(field)) or is_binary(field),
    do: "#{name}[#{field}]"

  @doc """
  Receives two forms structs and checks if the given field changed.

  The field will have changed if either its associated value, errors,
  action, or implementation changed. This is mostly used for optimization
  engines as an extension of the `Access` behaviour.
  """
  @spec input_changed?(t, t, field()) :: boolean()
  def input_changed?(
        %Form{
          impl: impl1,
          id: id1,
          name: name1,
          errors: errors1,
          source: source1,
          action: action1
        } = form1,
        %Form{
          impl: impl2,
          id: id2,
          name: name2,
          errors: errors2,
          source: source2,
          action: action2
        } = form2,
        field
      )
      when is_atom(field) or is_binary(field) do
    impl1 != impl2 or id1 != id2 or name1 != name2 or action1 != action2 or
      field_errors(errors1, field) != field_errors(errors2, field) or
      impl1.input_value(source1, form1, field) != impl2.input_value(source2, form2, field)
  end

  @doc """
  Returns the HTML validations that would apply to
  the given field.
  """
  @spec input_validations(t, field) :: Keyword.t()
  def input_validations(%{source: source, impl: impl} = form, field)
      when is_atom(field) or is_binary(field) do
    impl.input_validations(source, form, field)
  end

  @doc """
  Normalizes an input `value` according to its input `type`.

  Certain HTML input values must be cast, or they will have idiosyncracies
  when they are rendered. The goal of this function is to encapsulate
  this logic. In particular:

    * For "datetime-local" types, it converts `DateTime` and
      `NaiveDateTime` to strings without the second precision

    * For "checkbox" types, it returns a boolean depending on
      whether the input is "true" or not

    * For "textarea", it prefixes a newline to ensure newlines
      won't be ignored on submission. This requires however
      that the textarea is rendered with no spaces after its
      content
  """
  def normalize_value(type, value)

  def normalize_value("datetime-local", %struct{} = value)
      when struct in [NaiveDateTime, DateTime] do
    <<date::10-binary, ?\s, hour_minute::5-binary, _rest::binary>> = struct.to_string(value)
    {:safe, [date, ?T, hour_minute]}
  end

  def normalize_value("textarea", value) do
    {:safe, value} = html_escape(value || "")
    {:safe, [?\n | value]}
  end

  def normalize_value("checkbox", value) do
    html_escape(value) == {:safe, "true"}
  end

  def normalize_value(_type, value) do
    value
  end

  @doc """
  Returns options to be used inside a select element.

  `options` is expected to be an enumerable which will be used to
  generate each `option` element. The function supports different data
  for the individual elements:

    * keyword lists - each keyword list is expected to have the keys
      `:key` and `:value`. Additional keys such as `:disabled` may
      be given to customize the option.
    * two-item tuples - where the first element is an atom, string or
      integer to be used as the option label and the second element is
      an atom, string or integer to be used as the option value
    * simple atom, string or integer - which will be used as both label and value
      for the generated select

  ## Option groups

  If `options` is map or keyword list where the first element is a string,
  atom or integer and the second element is a list or a map, it is assumed
  the key will be wrapped in an `<optgroup>` and the value will be used to
  generate `<options>` nested under the group.

  ## Examples

      options_for_select(["Admin": "admin", "User": "user"], "admin")
      #=> <option value="admin" selected>Admin</option>
      #=> <option value="user">User</option>

  Multiple selected values:

      options_for_select(["Admin": "admin", "User": "user", "Moderator": "moderator"],
        ["admin", "moderator"])
      #=> <option value="admin" selected>Admin</option>
      #=> <option value="user">User</option>
      #=> <option value="moderator" selected>Moderator</option>

  Groups:

      options_for_select(["Europe": ["UK", "Sweden", "France"], ...], nil)
      #=> <optgroup label="Europe">
      #=>   <option>UK</option>
      #=>   <option>Sweden</option>
      #=>   <option>France</option>
      #=> </optgroup>

  Custom option tags:

      options_for_select(["Admin": "admin", "User": "user"], nil, tag: "opt")
      #=> <opt value="admin">Admin</opt>
      #=> <opt value="user">User</opt>

  Horizontal separators can be added:

      options_for_select(["Admin", "User", :hr, "New"], nil)
      #=> <option>Admin</option>
      #=> <option>User</option>
      #=> <hr/>
      #=> <option>New</option>

      options_for_select(["Admin": "admin", "User": "user", hr: nil, "New": "new"], nil)
      #=> <option value="admin" selected>Admin</option>
      #=> <option value="user">User</option>
      #=> <hr/>
      #=> <option value="new">New</option>


  """
  def options_for_select(options, selected_values, extra \\ []) do
    {:safe,
     escaped_options_for_select(
       options,
       selected_values |> List.wrap() |> Enum.map(&html_escape/1),
       extra
     )}
  end

  defp escaped_options_for_select(options, selected_values, extra) do
    Enum.reduce(options, [], fn
      {:hr, nil}, acc ->
        [acc | hr_tag()]

      {option_key, option_value}, acc ->
        [acc | option(option_key, option_value, extra, selected_values)]

      options, acc when is_list(options) ->
        {option_key, options} =
          case List.keytake(options, :key, 0) do
            nil ->
              raise ArgumentError,
                    "expected :key key when building <option> from keyword list: #{inspect(options)}"

            {{:key, key}, options} ->
              {key, options}
          end

        {option_value, options} =
          case List.keytake(options, :value, 0) do
            nil ->
              raise ArgumentError,
                    "expected :value key when building <option> from keyword list: #{inspect(options)}"

            {{:value, value}, options} ->
              {value, options}
          end

        [acc | option(option_key, option_value, extra ++ options, selected_values)]

      :hr, acc ->
        [acc | hr_tag()]

      option, acc ->
        [acc | option(option, option, extra, selected_values)]
    end)
  end

  defp option(group_label, group_values, extra, value)
       when is_list(group_values) or is_map(group_values) do
    section_options = escaped_options_for_select(group_values, value, extra)
    option_tag("optgroup", [label: group_label], {:safe, section_options})
  end

  defp option(option_key, option_value, extra, value) do
    option_key = html_escape(option_key)
    option_value = html_escape(option_value)
    attrs = extra ++ [selected: option_value in value, value: option_value]
    {tag, attrs} = Keyword.pop(attrs, :tag, "option")
    option_tag(tag, attrs, option_key)
  end

  defp option_tag(name, attrs, {:safe, body}) when is_binary(name) and is_list(attrs) do
    {:safe, attrs} = Phoenix.HTML.attributes_escape(attrs)
    [?<, name, attrs, ?>, body, ?<, ?/, name, ?>]
  end

  defp hr_tag() do
    [?<, "hr", ?/, ?>]
  end

  # Helper for getting field errors, handling string fields
  defp field_errors(errors, field)
       when is_list(errors) and (is_atom(field) or is_binary(field)) do
    for {^field, error} <- errors, do: error
  end
end
