# Installation

This guide installs the Popcorn 1.0 in a new Mix application.

We have separate guide for [JavaScript-first applications](TODO) (e.g. using Vite).
If you want to use Popcorn with Phoenix, please see [LocalLiveView](https://local-live-view.hexdocs.pm/) documentation.

## Requirements

- Elixir 1.19 or later
- OTP 29 or later
- `make` (Brotli library NIF), `inotify-tools` (Linux, for `popcorn.dev` FS watching)

These are versions that Popcorn uses for its VM. Try to match it. You can read more about Popcorn versioning in VM

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

## Next steps

Now you need an HTML page and some JavaScript. Continue with
["Hello World"](hello-world.md) for a minimal guide. To learn the
APIs and see Popcorn beyond bare-bones application, build the ["Hello counters"](hello-counters.md) example.
