defmodule Popdoc.Markdown do
  @moduledoc """
  ExDoc markdown processor that tags fenced code blocks preceded by
  `<!-- popcorn:eval -->` with the `popcorn-eval` class.

  It intentionally resolves ExDoc's markdown adapter at runtime so consumers
  can provide their own `:ex_doc` dependency.
  """

  @marker "popcorn:eval"
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
  defp walk({tag, attrs, children, meta}), do: {tag, attrs, walk(children), meta}
  defp walk(other), do: other

  defp walk_list(
         [
           {:comment, _, [text], %{comment: true}} = comment,
           {:pre, pre_attrs, pre_children, pre_meta} | rest
         ],
         acc
       ) do
    if String.trim(text) == @marker do
      pre = walk({:pre, add_class(pre_attrs, @eval_class), pre_children, pre_meta})
      walk_list(rest, [pre | acc])
    else
      pre = walk({:pre, pre_attrs, pre_children, pre_meta})
      walk_list(rest, [pre, comment | acc])
    end
  end

  defp walk_list([head | tail], acc), do: walk_list(tail, [walk(head) | acc])
  defp walk_list([], acc), do: Enum.reverse(acc)

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
