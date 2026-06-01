defmodule Popdoc.Markdown do
  @moduledoc """
  ExDoc markdown processor that tags fenced `elixir-popcorn` code blocks with
  the `popcorn-eval` class.

  It intentionally resolves ExDoc's markdown adapter at runtime so consumers
  can provide their own `:ex_doc` dependency.
  """

  @popcorn_code_class "elixir-popcorn"
  @eval_class "popcorn-eval"

  def available? do
    case earmark_adapter() do
      {:ok, module} -> apply(module, :available?, [])
      :error -> false
    end
  end

  def to_ast(text, opts) do
    earmark_adapter!()
    |> apply(:to_ast, [text, opts])
    |> walk()
  end

  defp walk(list) when is_list(list), do: walk_list(list, [])

  defp walk({:pre, pre_attrs, [{:code, code_attrs, code_children, code_meta}], pre_meta}) do
    if popcorn_code_block?(code_attrs) do
      {:pre, add_class(pre_attrs, @eval_class),
       [{:code, normalize_code_attrs(code_attrs), walk(code_children), code_meta}], pre_meta}
    else
      {:pre, pre_attrs, [{:code, code_attrs, walk(code_children), code_meta}], pre_meta}
    end
  end

  defp walk({tag, attrs, children, meta}), do: {tag, attrs, walk(children), meta}
  defp walk(other), do: other

  defp walk_list([head | tail], acc), do: walk_list(tail, [walk(head) | acc])
  defp walk_list([], acc), do: Enum.reverse(acc)

  defp popcorn_code_block?(attrs) do
    attrs
    |> class_tokens()
    |> Enum.member?(@popcorn_code_class)
  end

  defp normalize_code_attrs(attrs) do
    classes =
      attrs
      |> class_tokens()
      |> Enum.map(fn
        @popcorn_code_class -> "elixir"
        class -> class
      end)
      |> Enum.uniq()
      |> Enum.join(" ")

    Keyword.put(attrs, :class, classes)
  end

  defp class_tokens(attrs) do
    attrs
    |> Keyword.get(:class, "")
    |> String.split()
  end

  defp add_class(attrs, class) do
    case List.keytake(attrs, :class, 0) do
      nil -> [{:class, class} | attrs]
      {{:class, existing}, rest} -> [{:class, "#{existing} #{class}"} | rest]
    end
  end

  defp earmark_adapter do
    module = Module.concat([ExDoc, Markdown, Earmark])

    if Code.ensure_loaded?(module) do
      {:ok, module}
    else
      :error
    end
  end

  defp earmark_adapter! do
    case earmark_adapter() do
      {:ok, module} ->
        module

      :error ->
        raise """
        Popdoc requires ExDoc's Earmark adapter at docs runtime.
        Add `:ex_doc` to the consuming project's dependencies before running `mix docs`.
        """
    end
  end
end
