# Installation

LocalLiveView is installed into an existing Phoenix project using the `mix llv.install` generator.

## Prerequisites

- A Phoenix project generated with `mix phx.new`
- `:local_live_view` added as a dependency
- `npm` for JS package management

## Step 1 — Add the dependency

Add `:local_live_view` to your `mix.exs`:

```elixir
defp deps do
  [
    # ...existing deps...
    {:local_live_view, github: "software-mansion/popcorn", sparse: "local-live-view"}
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
| Adds COOP/COEP security headers (required for WASM) | `lib/*_web/endpoint.ex` |
| Registers `LocalLiveView.ChannelRegistry` | `lib/<app>/application.ex` |
| Imports `LocalLiveView.Component` | `lib/*_web.ex` (html_helpers) |
| Changes app.js script tag to `type="module"` | `lib/*_web/components/layouts/root.html.heex` |
| Adds `LLVEngine.create` call for the JS bridge | `assets/js/app.js` |
| Adds `local_live_view` JS package | `assets/package.json` |
| Replaces esbuild watcher with `build.mjs` | `mix.exs`, `config/dev.exs` |
| Generates the `local/` WASM project | `local/` |

> **Manual fallback:** If the installer can't find a file (e.g. your project has a non-standard structure), it prints the exact snippet to add manually.

## Step 3 — Setup project

```bash
mix setup
```

The installer already added `llv.build` to the `setup` alias in `mix.exs`. `llv.build` creates popcorn runtime for browser-side elixir.

This compiles your `local/` project to a WASM bundle at `priv/static/assets/js/wasm/bundle.avm`.


## Step 4 — Start the server

```bash
mix phx.server
```

The installer generated a sample `HelloLocal` view together with a page that renders it. Visit [localhost:4000/hello_local](http://localhost:4000/hello_local) to confirm everything works.

## What was generated

The installer creates a `local/` directory — a separate Mix project for your client-side Elixir code:

```
local/
├── config/
│   └── config.exs          # Popcorn output path config
├── lib/
│   ├── local/
│   │   └── application.ex  # OTP application
│   └── hello_local.ex      # Sample LocalLiveView
├── .formatter.exs
└── mix.exs                 # Compiles to WASM via popcorn.cook
```

Now you can add your LocalLiveView modules to `local/lib/`. When the server is running, the LLV watcher detects changes and automatically rebuilds the local code. To enforce rebuild, run `mix llv.build` and restart the server.

## Security headers

LocalLiveView requires [SharedArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer), which browsers only allow with the following HTTP headers set:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

The installer adds a `put_wasm_security_headers/2` plug to your endpoint automatically. If you use a CDN or reverse proxy, make sure these headers are forwarded.
