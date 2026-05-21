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
    coi_serviceworker: Mix.env() == :dev
  )
end
```

Then generate docs normally:

```bash
mix deps.get
mix docs
mix popdoc.server
```

`coi_serviceworker: true` is useful for local/debug docs hosting on `localhost`
when you cannot control response headers directly. For production deployments,
leave it disabled and serve real `Cross-Origin-Opener-Policy: same-origin` and
`Cross-Origin-Embedder-Policy: require-corp` headers from the web server.
