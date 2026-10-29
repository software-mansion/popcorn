# Installation

This guide installs Popcorn 1.0 in a new Mix application.

We have a separate guide for ["JavaScript-first applications"](hello-js.md) (e.g. using Vite).
If you want to use Popcorn with Phoenix, please see [LocalLiveView](https://hexdocs.pm/local_live_view/) documentation.

## Requirements

- Elixir 1.19 or later
- OTP 29, any release (the selected runtime release's major version)
- `make` (Brotli library NIF), `inotify-tools` (Linux, for `popcorn.dev` FS watching)

Use the toolchain pinned in [mise.toml](https://github.com/software-mansion/popcorn/blob/v1.0.0/mise.toml) for this release. Your local toolchain must be compatible with the one used to build the VM. See ["Compatibility and limits"](compatibility.md#compatibility) for details.

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
    {:popcorn, "1.0.0"}
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
APIs and see Popcorn beyond a bare-bones application, build the ["Hello counters"](hello-counters.md) example.
