# Folded examples

Use standalone `# exdoc:fold Label` and `# exdoc:endfold` comments inside
explicitly tagged Elixir fences with this repository's local ExDoc extension.
Labels are optional. Multiple regions are supported; nested, empty, and unmatched
regions fail documentation generation. Ordinary comments are preserved.
Copy includes the complete clean example. All code stays visible without
JavaScript, in print, and in noninteractive output formats.

```elixir
# exdoc:fold Setup
defmodule Example do
  # An ordinary comment.
  def run do
    values = [1, 2, 3]
# exdoc:endfold
    Enum.map(values, &(&1 * 2))
# exdoc:fold
    |> Enum.sum()
  end
end
# exdoc:endfold

message = """
Visible first line
# exdoc:fold More text
Hidden middle line
Another hidden line
# exdoc:endfold
Visible last line
"""
IO.puts(message)
```

The last region crosses a multiline string token. The markers are authoring
directives even inside literals: they are removed before highlighting and copying.
The clean example is highlighted as a whole.

## HTML preview

The label stays on the left. The code preview uses one indentation level less
than the next visible line.

```html
<!-- exdoc:fold `<html> ...` | HTML structure -->
<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
  </head>
<!-- exdoc:endfold -->
  <body>
    <p>Hello world!</p>
  </body>
</html>
```
