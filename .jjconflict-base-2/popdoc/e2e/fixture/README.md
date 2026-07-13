# Example

Minimal Popdoc fixture for manual docs checks.

## Single expression

```elixir-popcorn
Example.add(20, 22)
```

## Multiple expressions

```elixir-popcorn
x = Example.add(20, 22)
IO.puts("hello\nfrom\nstdout")
IO.puts(:stderr, "hello from stderr\nit even has multiple lines")
list = Enum.map(1..3, fn n -> n * x end)
case list do
  [_ | _] -> :non_empty
  [] -> :empty
end
```

## Error

```elixir-popcorn
a = 1
b = 0
all = {c, d} = {10, 20}
Process.sleep(2000)
div(a, b)
Example.hello()
```

## Error with stacktrace

```elixir-popcorn
Example.divide_all(10, [2, 5, 0])
```

Build the docs with:

```bash
mix deps.get
mix docs
mix popdoc.server
```

Then open `http://localhost:4000`.
