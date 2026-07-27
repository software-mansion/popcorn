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

## IEx session

Click an `iex>` prompt to run it in the on-page terminal, or open the terminal
any time with the floating `iex` button in the bottom-right corner. You can also
type freely there. `Clear` wipes the screen; `Reset` restarts the whole Popcorn
runtime (variables, modules, and eval-block state).

```iex-popcorn
iex> x = 1 + 1
2
iex> x * 10
20
```

```iex-popcorn
iex> Enum.sort([3, 2, 1])
[1, 2, 3]
iex> total =
...>   [10, 20, 30]
...>   |> Enum.sum()
60
iex> total * 2
120
```

Build the docs with:

```bash
mix deps.get
mix docs
mix popdoc.server
```

Then open `http://localhost:4000`.
