# Installation

This guide installs the Popcorn 0.4 in a new Mix application.

We have separate guide for [JavaScript-first applications](TODO) (e.g. using Vite). If you want to use Popcorn with Phoenix, please see [LocalLiveView](https://local-live-view.hexdocs.pm/) documentation.

## Requirements

- Elixir 1.19 or later
- OTP 29 or later

## Initialization

Create a new application with a supervision tree:

```sh
mix new foo --sup
```

## Adding the package

Add Popcorn to `mix.exs`:

```elixir
defp deps do
  [
    {:popcorn, "0.4.0"}
  ]
end
```

Get the dependency and compile the application:

```bash
mix deps.get
mix compile
```

That's it!

Now, you need a HTML page and some JS. Continue on [Hello world](TODO) for minimal guide. If you want to see different Popcorn APIs, check out [Hello counters](TODO).
