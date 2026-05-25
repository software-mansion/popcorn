# Example

Minimal Popdoc fixture for manual docs checks.

# Single expression

<!-- popcorn:eval -->

```elixir
Example.add(20, 22)
```

# Multiple expressions

<!-- popcorn:eval -->

```elixir
x = Example.add(20, 22)
IO.puts("hello from stdout")
IO.puts(:stderr, "hello from stderr")
list = Enum.map(1..3, fn n -> n * x end)
case list do
  [_ | _] -> :non_empty
  [] -> :empty
end
```

# Error

<!-- popcorn:eval -->

```elixir
a = 1
b = 0
Process.sleep(2000)
div(a, b)
Example.hello()
```

Build the docs with:

```bash
mix deps.get
mix docs
mix popdoc.server
```

Then open `http://localhost:4000`.
