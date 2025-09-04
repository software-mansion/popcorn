#defprotocol Phoenix.HTML.Safe do
#  @moduledoc """
#  Defines the HTML safe protocol.
#
#  In order to promote HTML safety, Phoenix templates
#  do not use `Kernel.to_string/1` to convert data types to
#  strings in templates. Instead, Phoenix uses this
#  protocol which must be implemented by data structures
#  and guarantee that a HTML safe representation is returned.
#
#  Furthermore, this protocol relies on iodata, which provides
#  better performance when sending or streaming data to the client.
#  """
#
#  def to_iodata(data)
#end

defmodule Phoenix.HTML.Safe do
  #def to_iodata(%{id: id, component: component}) do
  #      raise ArgumentError, """
  #      cannot convert component #{inspect(component)} with id #{inspect(id)} to HTML.
  #
  #      A component must always be returned directly as part of a LiveView template.
  #
  #      For example, this is not allowed:
  #
  #          <%= content_tag :div do %>
  #            <.live_component module={SomeComponent} id="myid" />
  #          <% end %>
  #
  #      That's because the component is inside `content_tag`. However, this works:
  #
  #          <div>
  #            <.live_component module={SomeComponent} id="myid" />
  #          </div>
  #
  #      Components are also allowed inside Elixir's special forms, such as
  #      `if`, `for`, `case`, and friends.
  #
  #          <%= for item <- items do %>
  #            <.live_component module={SomeComponent} id={item} />
  #          <% end %>
  #
  #      However, using other module functions such as `Enum`, will not work:
  #
  #          <%= Enum.map(items, fn item -> %>
  #            <.live_component module={SomeComponent} id={item} />
  #          <% end %>
  #      """
  #    end
  #  def to_iodata(%Phoenix.LiveView.Comprehension{static: static, entries: entries}) do
  #    for {_key, _vars, render} <- entries, do: to_iodata(static, render.(%{}, false))
  #  end
  #
  #  defp to_iodata([static_head | static_tail], [%_{} = struct | dynamic_tail]) do
  #    dynamic_head = Phoenix.HTML.Safe.to_iodata(struct)
  #    [static_head, dynamic_head | to_iodata(static_tail, dynamic_tail)]
  #  end
  #
  #  defp to_iodata([static_head | static_tail], [dynamic_head | dynamic_tail]) do
  #    [static_head, dynamic_head | to_iodata(static_tail, dynamic_tail)]
  #  end
  #
  #  defp to_iodata([static_head], []) do
  #    [static_head]
  #  end
  #  
  def to_iodata(%Phoenix.LiveView.Rendered{static: static, dynamic: dynamic}) do
    to_iodata(static, dynamic.(false), [])
  end

  def to_iodata(%_{} = struct) do
    Phoenix.HTML.Safe.to_iodata(struct)
  end

  def to_iodata(other) do
    other
  end

  defp to_iodata([static_head | static_tail], [dynamic_head | dynamic_tail], acc) do
    to_iodata(static_tail, dynamic_tail, [to_iodata(dynamic_head), static_head | acc])
  end

  defp to_iodata([static_head], [], acc) do
    Enum.reverse([static_head | acc])
  end
end

defmodule Phoenix.HTML.Safe.Atom do
  def to_iodata(nil), do: ""
  def to_iodata(atom), do: Phoenix.HTML.Engine.html_escape(Atom.to_string(atom))
end

#defimpl Phoenix.HTML.Safe, for: BitString do
#  defdelegate to_iodata(data), to: Phoenix.HTML.Engine, as: :html_escape
#end
#
#defimpl Phoenix.HTML.Safe, for: Time do
#  defdelegate to_iodata(data), to: Time, as: :to_iso8601
#end
#
#defimpl Phoenix.HTML.Safe, for: Date do
#  defdelegate to_iodata(data), to: Date, as: :to_iso8601
#end
#
#defimpl Phoenix.HTML.Safe, for: NaiveDateTime do
#  defdelegate to_iodata(data), to: NaiveDateTime, as: :to_iso8601
#end
#
#defimpl Phoenix.HTML.Safe, for: DateTime do
#  def to_iodata(data) do
#    # Call escape in case someone can inject reserved
#    # characters in the timezone or its abbreviation
#    Phoenix.HTML.Engine.html_escape(DateTime.to_iso8601(data))
#  end
#end

#if Code.ensure_loaded?(Duration) do
#  defimpl Phoenix.HTML.Safe, for: Duration do
#    defdelegate to_iodata(data), to: Duration, as: :to_iso8601
#  end
#end

defmodule Phoenix.HTML.Safe.List do
  def to_iodata(list), do: recur(list)

  defp recur([h | t]), do: [recur(h) | recur(t)]
  defp recur([]), do: []

  defp recur(?<), do: "&lt;"
  defp recur(?>), do: "&gt;"
  defp recur(?&), do: "&amp;"
  defp recur(?"), do: "&quot;"
  defp recur(?'), do: "&#39;"

  defp recur(h) when is_integer(h) and h <= 255 do
    h
  end

  defp recur(h) when is_integer(h) do
    raise ArgumentError,
          "lists in Phoenix.HTML templates only support iodata, and not chardata. Integers may only represent bytes. " <>
            "It's likely you meant to pass a string with double quotes instead of a char list with single quotes."
  end

  defp recur(h) when is_binary(h) do
    Phoenix.HTML.Engine.html_escape(h)
  end

  defp recur({:safe, data}) do
    data
  end

  defp recur(other) do
    raise ArgumentError,
          "lists in Phoenix.HTML and templates may only contain integers representing bytes, binaries or other lists, " <>
            "got invalid entry: #{inspect(other)}"
  end
end

#defimpl Phoenix.HTML.Safe, for: Integer do
#  defdelegate to_iodata(data), to: Integer, as: :to_string
#end
#
#defimpl Phoenix.HTML.Safe, for: Float do
#  defdelegate to_iodata(data), to: Float, as: :to_string
#end
#
#defimpl Phoenix.HTML.Safe, for: Tuple do
#  def to_iodata({:safe, data}), do: data
#  def to_iodata(value), do: raise(Protocol.UndefinedError, protocol: @protocol, value: value)
#end
#
#defimpl Phoenix.HTML.Safe, for: URI do
#  def to_iodata(data), do: Phoenix.HTML.Engine.html_escape(URI.to_string(data))
#end
