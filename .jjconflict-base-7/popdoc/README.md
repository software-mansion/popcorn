# Popdoc

ExDoc extension for interactive Elixir code evaluation via Popcorn.

## Installation

Add both `ex_doc` and `popdoc` to your docs toolchain:

```elixir
def deps do
  [
    {:ex_doc, "~> 0.40", only: :dev, runtime: false},
    {:popdoc, "~> 0.1.0", only: :dev, runtime: false}
  ]
end
```

Configure ExDoc lazily so `Popdoc` is resolved only when `mix docs` runs:

```elixir
def project do
  [
    app: :my_app,
    version: "0.1.0",
    elixir: "~> 1.17",
    deps: deps(),
    docs: &docs/0
  ]
end

defp docs do
  Popdoc.config(
    main: "readme",
    extras: ["README.md"],
    popdoc: [coi_serviceworker: Mix.env() == :dev]
  )
end
```

To make a block runnable, use the `elixir-popcorn` fence:

````
```elixir-popcorn
1+2
```
````

Then generate docs normally:

```bash
mix deps.get
mix docs
mix popdoc.server # to serve the docs locally. It's needed for CORS headers.
```

`popdoc: [coi_serviceworker: true]` is useful for docs hosting when you cannot control response headers directly (i.e. Github pages, Hexdocs). For production deployments, leave it disabled and serve real `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp` headers from the web server.
