defmodule Phoenix.HTML do
  @moduledoc """
  Building blocks for working with HTML in Phoenix.

  This library provides three main functionalities:

    * HTML safety
    * Form abstractions
    * A tiny JavaScript library to enhance applications

  ## HTML safety

  One of the main responsibilities of this package is to
  provide convenience functions for escaping and marking
  HTML code as safe.

  By default, data output in templates is not considered
  safe:

  ```heex
  <%= "<hello>" %>
  ```

  will be shown as:

  ```html
  &lt;hello&gt;
  ```

  User data or data coming from the database is almost never
  considered safe. However, in some cases, you may want to tag
  it as safe and show its "raw" contents:

  ```heex
  <%= raw "<hello>" %>
  ```

  ## Form handling

  See `Phoenix.HTML.Form`.

  ## JavaScript library

  This project ships with a tiny bit of JavaScript that listens
  to all click events to:

    * Support `data-confirm="message"` attributes, which shows
      a confirmation modal with the given message

    * Support `data-method="patch|post|put|delete"` attributes,
      which sends the current click as a PATCH/POST/PUT/DELETE
      HTTP request. You will need to add `data-to` with the URL
      and `data-csrf` with the CSRF token value

    * Dispatch a "phoenix.link.click" event. You can listen to this
      event to customize the behaviour above. Returning false from
      this event will disable `data-method`. Stopping propagation
      will disable `data-confirm`

  To use the functionality above, you must load `priv/static/phoenix_html.js`
  into your build tool.

  ### Overriding the default confirmation behaviour

  You can override the default implementation by hooking
  into `phoenix.link.click`. Here is an example:

  ```javascript
  window.addEventListener('phoenix.link.click', function (e) {
    // Introduce custom behaviour
    var message = e.target.getAttribute("data-prompt");
    var answer = e.target.getAttribute("data-prompt-answer");
    if(message && answer && (answer != window.prompt(message))) {
      e.preventDefault();
    }
  }, false);
  ```

  """

  @doc false
  defmacro __using__(_) do
    raise """
    use Phoenix.HTML is no longer supported in v4.0.

    To keep compatibility with previous versions, \
    add {:phoenix_html_helpers, "~> 1.0"} to your mix.exs deps
    and then, instead of "use Phoenix.HTML", you might:

        import Phoenix.HTML
        import Phoenix.HTML.Form
        use PhoenixHTMLHelpers

    """
  end

  @typedoc "Guaranteed to be safe"
  @type safe :: {:safe, iodata}

  @typedoc "May be safe or unsafe (i.e. it needs to be converted)"
  @type unsafe :: Phoenix.HTML.Safe.t()

  @doc """
  Marks the given content as raw.

  This means any HTML code inside the given
  string won't be escaped.

      iex> raw("<hello>")
      {:safe, "<hello>"}
      iex> raw({:safe, "<hello>"})
      {:safe, "<hello>"}
      iex> raw(nil)
      {:safe, ""}

  """
  @spec raw(iodata | safe | nil) :: safe
  def raw({:safe, value}), do: {:safe, value}
  def raw(nil), do: {:safe, ""}
  def raw(value) when is_binary(value) or is_list(value), do: {:safe, value}

  @doc """
  Escapes the HTML entities in the given term, returning safe iodata.

      iex> html_escape("<hello>")
      {:safe, [[[] | "&lt;"], "hello" | "&gt;"]}

      iex> html_escape(~c"<hello>")
      {:safe, ["&lt;", 104, 101, 108, 108, 111, "&gt;"]}

      iex> html_escape(1)
      {:safe, "1"}

      iex> html_escape({:safe, "<hello>"})
      {:safe, "<hello>"}

  """
  @spec html_escape(unsafe) :: safe
  def html_escape({:safe, _} = safe), do: safe
  def html_escape(other), do: {:safe, Phoenix.HTML.Engine.encode_to_iodata!(other)}

  @doc """
  Converts a safe result into a string.

  Fails if the result is not safe. In such cases, you can
  invoke `html_escape/1` or `raw/1` accordingly before.

  You can combine `html_escape/1` and `safe_to_string/1`
  to convert a data structure to a escaped string:

      data |> html_escape() |> safe_to_string()
  """
  @spec safe_to_string(safe) :: String.t()
  def safe_to_string({:safe, iodata}) do
    IO.iodata_to_binary(iodata)
  end

  @doc ~S"""
  Escapes an enumerable of attributes, returning iodata.

  The attributes are rendered in the given order. Note if
  a map is given, the key ordering is not guaranteed.

  The keys and values can be of any shape, as long as they
  implement the `Phoenix.HTML.Safe` protocol. In addition,
  if the key is an atom, it will be "dasherized". In other
  words, `:phx_value_id` will be converted to `phx-value-id`.

  Furthermore, the following attributes provide behaviour:

    * `:aria`, `:data`, and `:phx` - they accept a keyword list as
      value. `data: [confirm: "are you sure?"]` is converted to
      `data-confirm="are you sure?"`.

    * `:class` - it accepts a list of classes as argument. Each
      element in the list is separated by space. `nil` and `false`
      elements are discarded. `class: ["foo", nil, "bar"]` then
      becomes `class="foo bar"`.

    * `:id` - it is validated raise if a number is given as ID,
      which is not allowed by the HTML spec and leads to unpredictable
      behaviour.

  ## Examples

      iex> safe_to_string attributes_escape(title: "the title", id: "the id", selected: true)
      " title=\"the title\" id=\"the id\" selected"

      iex> safe_to_string attributes_escape(%{data: [confirm: "Are you sure?"]})
      " data-confirm=\"Are you sure?\""

      iex> safe_to_string attributes_escape(%{phx: [value: [foo: "bar"]]})
      " phx-value-foo=\"bar\""

  """
  def attributes_escape(attrs) when is_list(attrs) do
    {:safe, build_attrs(attrs)}
  end

  def attributes_escape(attrs) do
    {:safe, attrs |> Enum.to_list() |> build_attrs()}
  end

  defp build_attrs([{k, true} | t]),
    do: [?\s, key_escape(k) | build_attrs(t)]

  defp build_attrs([{_, false} | t]),
    do: build_attrs(t)

  defp build_attrs([{_, nil} | t]),
    do: build_attrs(t)

  defp build_attrs([{:id, v} | t]),
    do: [" id=\"", id_value(v), ?" | build_attrs(t)]

  defp build_attrs([{:class, v} | t]),
    do: [" class=\"", class_value(v), ?" | build_attrs(t)]

  defp build_attrs([{:aria, v} | t]) when is_list(v),
    do: nested_attrs(v, " aria", t)

  defp build_attrs([{:data, v} | t]) when is_list(v),
    do: nested_attrs(v, " data", t)

  defp build_attrs([{:phx, v} | t]) when is_list(v),
    do: nested_attrs(v, " phx", t)

  defp build_attrs([{"id", v} | t]),
    do: [" id=\"", id_value(v), ?" | build_attrs(t)]

  defp build_attrs([{"class", v} | t]),
    do: [" class=\"", class_value(v), ?" | build_attrs(t)]

  defp build_attrs([{"aria", v} | t]) when is_list(v),
    do: nested_attrs(v, " aria", t)

  defp build_attrs([{"data", v} | t]) when is_list(v),
    do: nested_attrs(v, " data", t)

  defp build_attrs([{"phx", v} | t]) when is_list(v),
    do: nested_attrs(v, " phx", t)

  defp build_attrs([{k, v} | t]),
    do: [?\s, key_escape(k), ?=, ?", attr_escape(v), ?" | build_attrs(t)]

  defp build_attrs([]), do: []

  defp nested_attrs([{k, true} | kv], attr, t),
    do: [attr, ?-, key_escape(k) | nested_attrs(kv, attr, t)]

  defp nested_attrs([{_, falsy} | kv], attr, t) when falsy in [false, nil],
    do: nested_attrs(kv, attr, t)

  defp nested_attrs([{k, v} | kv], attr, t) when is_list(v),
    do: [nested_attrs(v, "#{attr}-#{key_escape(k)}", []) | nested_attrs(kv, attr, t)]

  defp nested_attrs([{k, v} | kv], attr, t),
    do: [attr, ?-, key_escape(k), ?=, ?", attr_escape(v), ?" | nested_attrs(kv, attr, t)]

  defp nested_attrs([], _attr, t),
    do: build_attrs(t)

  defp id_value(value) when is_number(value) do
    raise ArgumentError,
          "attempting to set id attribute to #{value}, " <>
            "but setting the DOM ID to a number can lead to unpredictable behaviour. " <>
            "Instead consider prefixing the id with a string, such as \"user-#{value}\" or similar"
  end

  defp id_value(value) do
    attr_escape(value)
  end

  defp class_value(value) when is_list(value) do
    value
    |> list_class_value()
    |> attr_escape()
  end

  defp class_value(value) do
    attr_escape(value)
  end

  defp list_class_value(value) do
    value
    |> Enum.flat_map(fn
      nil -> []
      false -> []
      inner when is_list(inner) -> [list_class_value(inner)]
      other -> [other]
    end)
    |> Enum.join(" ")
  end

  defp key_escape(value) when is_atom(value), do: String.replace(Atom.to_string(value), "_", "-")
  defp key_escape(value), do: attr_escape(value)

  defp attr_escape({:safe, data}), do: data
  defp attr_escape(nil), do: []
  defp attr_escape(other) when is_binary(other), do: Phoenix.HTML.Engine.html_escape(other)
  defp attr_escape(other), do: Phoenix.HTML.Safe.to_iodata(other)

  @doc """
  Escapes HTML content to be inserted into a JavaScript string.

  This function is useful in JavaScript responses when there is a need
  to escape HTML rendered from other templates, like in the following:

      $("#container").append("<%= javascript_escape(render("post.html", post: @post)) %>");

  It escapes quotes (double and single), double backslashes and others.
  """
  @spec javascript_escape(binary) :: binary
  @spec javascript_escape(safe) :: safe
  def javascript_escape({:safe, data}),
    do: {:safe, data |> IO.iodata_to_binary() |> javascript_escape("")}

  def javascript_escape(data) when is_binary(data),
    do: javascript_escape(data, "")

  defp javascript_escape(<<0x2028::utf8, t::binary>>, acc),
    do: javascript_escape(t, <<acc::binary, "\\u2028">>)

  defp javascript_escape(<<0x2029::utf8, t::binary>>, acc),
    do: javascript_escape(t, <<acc::binary, "\\u2029">>)

  defp javascript_escape(<<0::utf8, t::binary>>, acc),
    do: javascript_escape(t, <<acc::binary, "\\u0000">>)

  defp javascript_escape(<<"</", t::binary>>, acc),
    do: javascript_escape(t, <<acc::binary, ?<, ?\\, ?/>>)

  defp javascript_escape(<<"\r\n", t::binary>>, acc),
    do: javascript_escape(t, <<acc::binary, ?\\, ?n>>)

  defp javascript_escape(<<h, t::binary>>, acc) when h in [?", ?', ?\\, ?`],
    do: javascript_escape(t, <<acc::binary, ?\\, h>>)

  defp javascript_escape(<<h, t::binary>>, acc) when h in [?\r, ?\n],
    do: javascript_escape(t, <<acc::binary, ?\\, ?n>>)

  defp javascript_escape(<<h, t::binary>>, acc),
    do: javascript_escape(t, <<acc::binary, h>>)

  defp javascript_escape(<<>>, acc), do: acc

  @doc """
  Escapes a string for use as a CSS identifier.

  ## Examples

      iex> css_escape("hello world")
      "hello\\\\ world"

      iex> css_escape("-123")
      "-\\\\31 23"

  """
  @spec css_escape(String.t()) :: String.t()
  def css_escape(value) when is_binary(value) do
    # This is a direct translation of
    # https://github.com/mathiasbynens/CSS.escape/blob/master/css.escape.js
    # into Elixir.
    value
    |> String.to_charlist()
    |> escape_css_chars()
    |> IO.iodata_to_binary()
  end

  defp escape_css_chars(chars) do
    case chars do
      # If the character is the first character and is a `-` (U+002D), and
      # there is no second character, […]
      [?- | []] -> ["\\-"]
      _ -> escape_css_chars(chars, 0, [])
    end
  end

  defp escape_css_chars([], _, acc), do: Enum.reverse(acc)

  defp escape_css_chars([char | rest], index, acc) do
    escaped =
      cond do
        # If the character is NULL (U+0000), then the REPLACEMENT CHARACTER
        # (U+FFFD).
        char == 0 ->
          <<0xFFFD::utf8>>

        # If the character is in the range [\1-\1F] (U+0001 to U+001F) or is
        # U+007F,
        # if the character is the first character and is in the range [0-9]
        # (U+0030 to U+0039),
        # if the character is the second character and is in the range [0-9]
        # (U+0030 to U+0039) and the first character is a `-` (U+002D),
        char in 0x0001..0x001F or char == 0x007F or
          (index == 0 and char in ?0..?9) or
            (index == 1 and char in ?0..?9 and hd(acc) == "-") ->
          # https://drafts.csswg.org/cssom/#escape-a-character-as-code-point
          ["\\", Integer.to_string(char, 16), " "]

        # If the character is not handled by one of the above rules and is
        # greater than or equal to U+0080, is `-` (U+002D) or `_` (U+005F), or
        # is in one of the ranges [0-9] (U+0030 to U+0039), [A-Z] (U+0041 to
        # U+005A), or [a-z] (U+0061 to U+007A), […]
        char >= 0x0080 or char in [?-, ?_] or char in ?0..?9 or char in ?A..?Z or char in ?a..?z ->
          # the character itself
          <<char::utf8>>

        true ->
          # Otherwise, the escaped character.
          # https://drafts.csswg.org/cssom/#escape-a-character
          ["\\", <<char::utf8>>]
      end

    escape_css_chars(rest, index + 1, [escaped | acc])
  end
end
