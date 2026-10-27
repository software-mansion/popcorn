# Installation

LocalLiveView is installed into an existing Phoenix project using the `mix llv.install` generator.

## Prerequisites

- A Phoenix project generated with `mix phx.new`
- Elixir 1.19 or later, and the Erlang/OTP version required by [Popcorn](https://popcorn.hexdocs.pm/1.0.0/compatibility.html#compatibility)
- `:local_live_view` added as a dependency
- The standard Phoenix esbuild setup for JavaScript assets

## Step 1 — Add the dependency

Add `:local_live_view` to your `mix.exs`:

```elixir
defp deps do
  [
    # ...existing deps...
    {:local_live_view, "~> 0.1.0"}
  ]
end
```

Then fetch it:

```bash
mix deps.get
```

## Step 2 — Run the installer

```bash
mix llv.install
```

The installer configures your project automatically:

| What | Where |
|---|---|
| Adds `LocalLiveView.Socket` | `lib/*_web/endpoint.ex` |
| Adds COOP/COEP security headers (required for Wasm) | `lib/*_web/endpoint.ex` |
| Adds the `local/` project as a dependency for server rendering | `mix.exs` |
| Imports `LocalLiveView.Component` | `lib/*_web.ex` (html_helpers) |
| Changes app.js script tag to `type="module"` | `lib/*_web/components/layouts/root.html.heex` |
| Adds `LLVEngine.create` call for the JS bridge | `assets/js/app.js` |
| Uses the `local_live_view` JS bundle shipped in the Mix dependency | `assets/js/app.js` |
| Configures esbuild for ESM output and adds the LLV watcher | `config/config.exs`, `config/dev.exs` |
| Generates the `local/` Wasm project | `local/` |

> **Manual fallback:** If the installer can't find a file (e.g. your project has a non-standard structure), it prints the exact snippet to add manually.

## Step 3 — Set up the project

```bash
mix setup
```

The installer already added `llv.build` to the `setup` alias in `mix.exs`. `llv.build` creates the Popcorn runtime for browser-side Elixir.

This compiles your `local/` project and packages it, together with the Popcorn Wasm runtime, into `priv/static/assets/js/popcorn/`. LocalLiveView's JS loads it from there: the `popcorn/` directory next to the `app.js` bundle.


## Step 4 — Start the server

```bash
mix phx.server
```

The installer generated a sample `HelloLocal` view together with a page that renders it. Visit [localhost:4000/hello_local](http://localhost:4000/hello_local) to confirm everything works.

## What was generated

The installer creates a `local/` directory — a separate Mix project for your client-side Elixir code:

```
local/
├── lib/
│   ├── local/
│   │   └── application.ex  # OTP application
│   └── hello_local.ex      # Sample LocalLiveView
├── .formatter.exs
└── mix.exs                 # Packaged for the browser by `mix llv.build`
```

Now you can add your LocalLiveView modules to `local/lib/`. When the server is running, the LLV watcher detects changes and automatically rebuilds the local code. To force a rebuild, run `mix llv.build` and restart the server.

## Security headers

LocalLiveView requires [SharedArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer), which browsers only allow with the following HTTP headers set:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

The installer adds a `put_wasm_security_headers/2` plug to your endpoint automatically. If you use a CDN or reverse proxy, make sure these headers are forwarded.
