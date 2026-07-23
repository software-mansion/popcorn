defmodule Popdoc.Markdown do
  @moduledoc """
  ExDoc markdown processor that tags fenced `elixir-popcorn` code blocks with
  the `popcorn-eval` class.

  It intentionally resolves ExDoc's markdown adapter at runtime so consumers
  can provide their own `:ex_doc` dependency.
  """

  @popcorn_code_class "elixir-popcorn"
  @eval_class "popcorn-eval"

  @iex_code_class "iex-popcorn"
  @iex_class "popcorn-iex"

  @iex_prompt_re ~r/^iex(?:\(\d+\))?> ?/
  @cont_prompt_re ~r/^\.\.\.(?:\(\d+\))?> ?/

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
    {pre_attrs, code_attrs} =
      cond do
        popcorn_code_block?(code_attrs) ->
          {add_class(pre_attrs, @eval_class), normalize_code_attrs(code_attrs)}

        iex_code_block?(code_attrs) ->
          commands_json =
            code_children |> extract_text() |> parse_iex_commands() |> commands_to_json()

          new_pre_attrs =
            pre_attrs
            |> add_class(@iex_class)
            |> Keyword.put(:"data-popcorn-iex-commands", commands_json)

          {new_pre_attrs, normalize_code_attrs(code_attrs)}

        true ->
          {pre_attrs, code_attrs}
      end

    {:pre, pre_attrs, [{:code, code_attrs, walk(code_children), code_meta}], pre_meta}
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

  defp iex_code_block?(attrs) do
    attrs
    |> class_tokens()
    |> Enum.member?(@iex_code_class)
  end

  defp normalize_code_attrs(attrs) do
    classes =
      attrs
      |> class_tokens()
      |> Enum.map(fn
        @popcorn_code_class -> "elixir"
        @iex_code_class -> "elixir"
        class -> class
      end)
      |> Enum.uniq()
      |> Enum.join(" ")

    Keyword.put(attrs, :class, classes)
  end

  defp extract_text(children) when is_list(children) do
    Enum.map_join(children, "", fn
      text when is_binary(text) -> text
      _ -> ""
    end)
  end

  defp parse_iex_commands(text) do
    {commands, current} =
      text
      |> String.split("\n")
      |> Enum.reduce({[], nil}, fn line, {commands, current} ->
        trimmed = String.trim_leading(line)

        cond do
          code = strip_prompt(trimmed, @iex_prompt_re) ->
            acc = if current != nil, do: [current | commands], else: commands
            {acc, code}

          (cont = strip_prompt(trimmed, @cont_prompt_re)) && current != nil ->
            {commands, current <> "\n" <> cont}

          true ->
            {commands, current}
        end
      end)

    final = if current != nil, do: [current | commands], else: commands
    Enum.reverse(final)
  end

  defp strip_prompt(line, prompt_re) do
    case Regex.run(prompt_re, line) do
      [prompt] -> String.replace_prefix(line, prompt, "")
      nil -> nil
    end
  end

  # ExDoc.Utils.to_json escapes every control character — a raw byte < 0x20
  # in a doc line would otherwise make JSON.parse throw and silently disable
  # the block. ExDoc is guaranteed loaded here: to_ast/2 already requires its
  # Earmark adapter.
  defp commands_to_json(commands) do
    Module.concat([ExDoc, Utils])
    |> apply(:to_json, [commands])
    |> IO.iodata_to_binary()
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
