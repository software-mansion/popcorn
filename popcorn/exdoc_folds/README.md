# ExDoc folds

Hide setup code in examples. Readers can expand one region or all regions in a code block.
The copy button always copies the complete example without the marker lines.

## Setup

Add the local package to your Mix dependencies:

```elixir
{:exdoc_folds, path: "../exdoc_folds", only: [:dev, :test], runtime: false}
```

Apply `ExDocFolds.configure/1` to your ExDoc options:

```elixir
[main: "MyApp"]
|> ExDocFolds.configure()
```

The extension requires ExDoc with its default Earmark processor. It preserves your existing Markdown options.

## Mark a region

Use paired marker comments in a code fence:

````markdown
```elixir
# exdoc:fold Setup
values = [1, 2, 3]
# exdoc:endfold

Enum.sum(values)
```
````

Use the comment form for your example. Supported line prefixes include `#`, `//`, `--`, `;`, `%`, `'`, `"`, and `REM`.
Repeated prefixes, such as `///` and `;;`, also work.
Supported block forms include `/* ... */`, `<!-- ... -->`, `(* ... *)`, and `{- ... -}`.
Each block marker must open and close its comment on the same line.

Add a code preview before the label with a backtick pair and a vertical bar:

```html
<!-- exdoc:fold `<html> ...` | HTML structure -->
<html>
  <head></head>
<!-- exdoc:endfold -->
  <body>...</body>
</html>
```

The label appears above the code preview. The preview uses one indentation level less than the next visible line. The chevron and label stay on the left.
The copy button excludes the label and preview.

The label is optional. Each marker must occupy a separate line.
You can mark multiple regions in one example. Do not nest regions or mark an empty region.
Invalid markers stop documentation generation with an error.

The extension removes marker lines, including markers inside multiline strings. It preserves ordinary comments and syntax colors.

## Other output formats

HTML without JavaScript, EPUB, and printed pages show the complete example without marker lines.
Raw Markdown output includes the complete example with the original marker comments.
