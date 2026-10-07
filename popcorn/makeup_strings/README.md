# MakeupStrings

Highlights string arguments of configured Elixir function calls with any lexer
registered in Makeup. It wraps the standard Makeup Elixir lexer and runs while
ExDoc generates HTML, so the target lexer is not shipped to the browser.

## Installation

Add `makeup_strings` and the lexer for the embedded language as development
dependencies:

```elixir
def deps do
  [
    {:makeup_strings, path: "../makeup_strings", only: :dev, runtime: false},
    {:makeup_syntect, "~> 0.1.4", only: :dev, runtime: false}
  ]
end
```

Configure registered Makeup language names and the functions whose string
arguments contain that language:

```elixir
config :makeup_strings,
  languages: [
    javascript: [
      {Popcorn.Wasm, :run_js, 3},
      {Popcorn.Wasm, :run_js!, 3}
    ]
  ]
```

The configured arity is the maximum call arity, so the example matches calls
to `run_js/1`, `run_js/2`, and `run_js/3`. Every direct argument that is a
complete non-interpolated string or heredoc is highlighted. Strings nested in
maps or other expressions remain ordinary Elixir strings. Standard aliases
such as `alias Popcorn.Wasm` followed by `Wasm.run_js/1` are resolved.

`makeup_strings` registers its composite lexer as `elixir` and `iex`. Language
names must already exist in the Makeup registry; for example,
`makeup_syntect` registers `javascript` and `typescript`.

Strings and heredocs piped directly into a configured call are also highlighted,
on the same line or across lines:

```elixir
"console.log(1)" |> Popcorn.Wasm.run_js()

"""
console.log(1);
"""
|> Popcorn.Wasm.run_js!(%{})
```

The piped value counts as the first argument for the configured arity limit.
