defmodule MakeupStrings.ElixirLexer do
  @moduledoc """
  Highlights configured string arguments with another registered Makeup lexer.

  Each configured MFA uses its arity as the maximum call arity. For example,
  `{Popcorn.JS, :run, 3}` matches `run/1`, `run/2`, and `run/3`.
  Piped values count as the first argument.
  """

  @behaviour Makeup.Lexer

  alias Makeup.Lexers.ElixirLexer

  @closing_delimiters [")", "]", "}", ">>", "end"]

  @impl Makeup.Lexer
  def lex(text, opts \\ []) do
    {languages, opts} =
      Keyword.pop(opts, :languages, Application.get_env(:makeup_strings, :languages, []))

    tokens = ElixirLexer.lex(text, opts)

    highlight_calls(tokens, languages, aliases(tokens))
  end

  @impl Makeup.Lexer
  def root(text), do: ElixirLexer.root(text)

  @impl Makeup.Lexer
  def root_element(text), do: ElixirLexer.root_element(text)

  @impl Makeup.Lexer
  def postprocess(tokens, opts \\ []), do: ElixirLexer.postprocess(tokens, opts)

  @impl Makeup.Lexer
  def match_groups(tokens, prefix), do: ElixirLexer.match_groups(tokens, prefix)

  defp highlight_calls(tokens, languages, aliases),
    do: highlight_calls(tokens, languages, aliases, [])

  defp highlight_calls([], _languages, _aliases, acc), do: Enum.reverse(acc)

  defp highlight_calls(
         [
           {:name_class, _, module} = module_token,
           {:operator, _, "."} = dot,
           {:name, _, function} = function_token,
           {:punctuation, %{group_id: group_id}, "("} = open
           | rest
         ],
         languages,
         aliases,
         acc
       ) do
    module = IO.iodata_to_binary(module)

    case take_call(rest, group_id) do
      {:ok, arguments, close, tail} ->
        piped? = match?([{:operator, _, "|>"} | _], Enum.drop_while(acc, &whitespace?/1))
        call_arity = arity(arguments) + if(piped?, do: 1, else: 0)

        language =
          configured_language(languages, Map.get(aliases, module, module), function, call_arity)

        acc = if language && piped?, do: highlight_piped(acc, language), else: acc

        arguments =
          if language do
            highlight_arguments(arguments, language, languages, aliases)
          else
            highlight_calls(arguments, languages, aliases)
          end

        call = [module_token, dot, function_token, open] ++ arguments ++ [close]
        highlight_calls(tail, languages, aliases, Enum.reverse(call, acc))

      :error ->
        highlight_calls([dot, function_token, open | rest], languages, aliases, [
          module_token | acc
        ])
    end
  end

  defp highlight_calls([token | rest], languages, aliases, acc),
    do: highlight_calls(rest, languages, aliases, [token | acc])

  defp highlight_piped(tokens, language) do
    {after_pipe, [{:operator, _, "|>"} = pipe | rest]} =
      Enum.split_while(tokens, &whitespace?/1)

    {before_pipe, rest} = Enum.split_while(rest, &whitespace?/1)

    case rest do
      [{:string, meta, value} = token | tail] ->
        after_pipe ++
          [pipe | before_pipe] ++
          Enum.reverse(highlight_string(token, meta, IO.iodata_to_binary(value), language), tail)

      _ ->
        tokens
    end
  end

  defp aliases(tokens), do: aliases(tokens, %{})

  defp aliases(
         [
           {:keyword_namespace, _, "alias"},
           {:whitespace, _, _},
           {:name_class, _, module}
           | rest
         ],
         aliases
       ) do
    module = IO.iodata_to_binary(module)
    alias_name = module |> String.split(".") |> List.last()

    aliases(rest, Map.put(aliases, alias_name, module))
  end

  defp aliases([_token | rest], aliases), do: aliases(rest, aliases)
  defp aliases([], aliases), do: aliases

  defp take_call(tokens, group_id) do
    case Enum.split_while(tokens, fn
           {:punctuation, %{group_id: ^group_id}, ")"} -> false
           _ -> true
         end) do
      {arguments, [close | tail]} -> {:ok, arguments, close, tail}
      _ -> :error
    end
  end

  defp configured_language(languages, module, function, arity) do
    Enum.find_value(languages, fn {language, mfas} ->
      Enum.find(mfas, fn
        {configured_module, configured_function, max_arity}
        when is_atom(configured_module) and is_atom(configured_function) and
               is_integer(max_arity) and max_arity > 0 ->
          inspect(configured_module) == module and
            Atom.to_string(configured_function) == function and arity in 1..max_arity

        invalid ->
          raise ArgumentError, "invalid configured MFA: #{inspect(invalid)}"
      end) && language
    end)
  end

  defp arity(tokens) do
    tokens
    |> split_arguments()
    |> Enum.count(fn
      {:argument, tokens} -> Enum.any?(tokens, &(not whitespace?(&1)))
      {:separator, _token} -> false
    end)
  end

  defp highlight_arguments(tokens, language, languages, aliases) do
    tokens
    |> split_arguments()
    |> Enum.flat_map(fn
      {:argument, argument} -> highlight_argument(argument, language, languages, aliases)
      {:separator, token} -> [token]
    end)
  end

  defp split_arguments(tokens) do
    {parts, argument, _groups} =
      Enum.reduce(tokens, {[], [], []}, fn token, {parts, argument, groups} ->
        if comma?(token) and groups == [] do
          {[{:separator, token}, {:argument, Enum.reverse(argument)} | parts], [], groups}
        else
          {parts, [token | argument], update_groups(groups, token)}
        end
      end)

    Enum.reverse([{:argument, Enum.reverse(argument)} | parts])
  end

  defp update_groups(groups, {_type, %{group_id: group_id}, value}) do
    if IO.iodata_to_binary(value) in @closing_delimiters do
      List.delete(groups, group_id)
    else
      [group_id | groups]
    end
  end

  defp update_groups(groups, _token), do: groups

  defp comma?({:punctuation, _, ","}), do: true
  defp comma?(_token), do: false

  defp highlight_argument(tokens, language, languages, aliases) do
    {leading, rest} = Enum.split_while(tokens, &whitespace?/1)
    {trailing, core} = rest |> Enum.reverse() |> Enum.split_while(&whitespace?/1)

    case core do
      [{:string, meta, value} = token] ->
        leading ++
          highlight_string(token, meta, IO.iodata_to_binary(value), language) ++
          Enum.reverse(trailing)

      _ ->
        highlight_calls(tokens, languages, aliases)
    end
  end

  defp whitespace?({:whitespace, _, _}), do: true
  defp whitespace?(_token), do: false

  defp highlight_string(token, meta, "\"\"\"" <> rest, language),
    do: highlight_delimited(token, meta, rest, "\"\"\"", language)

  defp highlight_string(token, meta, "\"" <> rest, language),
    do: highlight_delimited(token, meta, rest, "\"", language)

  defp highlight_string(token, _meta, _value, _language), do: [token]

  defp highlight_delimited(token, meta, rest, delimiter, language) do
    if String.ends_with?(rest, delimiter) do
      content_size = byte_size(rest) - byte_size(delimiter)
      content = binary_part(rest, 0, content_size)
      {lexer, opts} = Makeup.Registry.fetch_lexer_by_name!(to_string(language))

      [
        {:string, Map.put(meta, :group_id, "makeup-string-start"), delimiter},
        lexer.lex(content, opts),
        {:string, Map.put(meta, :group_id, "makeup-string-end"), delimiter}
      ]
      |> List.flatten()
    else
      [token]
    end
  end
end
