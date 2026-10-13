defmodule ExDocFolds do
  @moduledoc """
  Local ExDoc Markdown adapter for standalone `# exdoc:fold Label` and
  `# exdoc:endfold` comments in code examples. Labels are optional;
  regions cannot nest. Marker errors stop documentation generation.
  """

  @compile {:no_warn_undefined, ExDoc.Markdown.Earmark}

  def configure(docs) do
    assets =
      Map.put(Keyword.get(docs, :assets, %{}), Path.expand("../assets", __DIR__), "exdoc-folds")

    {ExDoc.Markdown.Earmark, options} =
      case Keyword.get(docs, :markdown_processor, ExDoc.Markdown.Earmark) do
        {processor, options} -> {processor, options}
        processor -> {processor, []}
      end

    docs
    |> Keyword.put(:assets, assets)
    |> Keyword.put(:markdown_processor, {__MODULE__, options})
    |> hook(:before_closing_head_tag, """
    <style>
      @media screen {
        pre[data-exdoc-folds]:not([data-folds-ready]) {
          visibility: hidden;
          animation: exdoc-folds-fallback 2s step-end both;
        }
      }
      @keyframes exdoc-folds-fallback {
        from { visibility: hidden; }
        to { visibility: visible; }
      }
    </style>
    <noscript><style>
      pre[data-exdoc-folds]:not([data-folds-ready]) { visibility: visible; animation: none; }
    </style></noscript>
    <link rel="stylesheet" href="exdoc-folds/folds.css">
    """)
    |> hook(:before_closing_body_tag, ~s(<script src="exdoc-folds/folds.js"></script>))
  end

  defp hook(docs, key, html) do
    previous = Keyword.get(docs, key, fn _ -> "" end)

    Keyword.put(docs, key, fn format ->
      previous.(format) <> if(format == :html, do: html, else: "")
    end)
  end

  def available?, do: ExDoc.Markdown.Earmark.available?()

  def to_ast(text, opts) do
    text |> ExDoc.Markdown.Earmark.to_ast(opts) |> transform(opts)
  end

  defp transform({:pre, attrs, [{:code, code_attrs, [source], code_meta}], meta}, opts) do
    {source, folds} = parse(source, opts)
    attrs = if folds == "", do: attrs, else: [{:"data-exdoc-folds", folds} | attrs]
    {:pre, attrs, [{:code, code_attrs, [source], code_meta}], meta}
  end

  defp transform({tag, attrs, children, meta}, opts),
    do: {tag, attrs, transform(children, opts), meta}

  defp transform(nodes, opts) when is_list(nodes), do: Enum.map(nodes, &transform(&1, opts))
  defp transform(node, _opts), do: node

  def parse(source, opts \\ []) do
    {lines, folds, open, _} =
      source
      |> String.split("\n")
      |> Enum.with_index(1)
      |> Enum.reduce({[], [], nil, 0}, fn {line, number}, {lines, folds, open, count} ->
        case marker(line, opts, number) do
          "exdoc:endfold" ->
            if is_nil(open), do: error!(opts, number, "endfold without fold")
            {start, label} = open
            if start == count, do: error!(opts, number, "empty fold")
            fold = "#{start}:#{count}:#{Base.encode64(label)}"
            {lines, [fold | folds], nil, count}

          "exdoc:fold" <> label ->
            if open, do: error!(opts, number, "nested fold")
            {lines, folds, {count, String.trim(label)}, count}

          nil ->
            {[line | lines], folds, open, count + 1}
        end
      end)

    if open, do: error!(opts, length(String.split(source, "\n")), "unclosed fold")
    {lines |> Enum.reverse() |> Enum.join("\n"), folds |> Enum.reverse() |> Enum.join(";")}
  end

  defp marker(line, opts, number) do
    case Regex.run(
           ~r/^\s*(<!--|\/\*+|\(\*|\{-|\/{2,}|\#+|--+|;+|%+|'|"|REM\b)\s*(exdoc:.*)$/i,
           line
         ) do
      [_, comment, marker] ->
        closing =
          case comment do
            "<!--" -> "-->"
            "/*" <> _ -> "*/"
            "(*" -> "*)"
            "{-" -> "-}"
            _ -> ""
          end

        marker = String.trim(marker)

        unless String.ends_with?(marker, closing),
          do: error!(opts, number, "unclosed marker comment")

        marker =
          if closing == "",
            do: marker,
            else: marker |> String.trim_trailing(closing) |> String.trim()

        unless Regex.match?(~r/^exdoc:(?:fold(?:\s+.*)?|endfold)$/, marker),
          do: error!(opts, number, "malformed fold marker")

        marker

      nil ->
        nil
    end
  end

  defp error!(opts, line, message) do
    raise ArgumentError, "#{opts[:file] || "nofile"}: code line #{line}: exdoc folds: #{message}"
  end
end
