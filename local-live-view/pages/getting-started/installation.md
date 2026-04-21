# Installation

LocalLiveView is installed into an existing Phoenix project using the `mix llv.install` generator.

## Prerequisites

- A Phoenix project generated with `mix phx.new`
- `:local_live_view` added as a dependency
- `pnpm` or `npm` for JS package management

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
| Adds `setup` call for the JS bridge | `assets/js/app.js` |
| Adds `local_live_view` JS package | `assets/package.json` |
| Replaces esbuild watcher with `build.mjs` | `mix.exs`, `config/dev.exs` |
| Generates the `local/` WASM project | `local/` |

> **Manual fallback:** If the installer can't find a file (e.g. your project has a non-standard structure), it prints the exact snippet to add manually.

## Step 3 — Install JS dependencies

```bash
pnpm install
# or: npm install --prefix assets
```

## Step 4 — Build the WASM bundle

```bash
mix llv.build
```

This compiles your `local/` project to a WASM bundle at `priv/static/assets/js/wasm/bundle.avm`.

To build automatically as part of `mix setup`, the installer already adds `llv.build` to the `setup` alias in `mix.exs`.

## Step 5 — Start the server

```bash
mix phx.server
```

Visit [localhost:4000](http://localhost:4000). The installer generated a sample `HelloLocal` view — add it to any template to confirm everything works:

```heex
<.local_live_view view="HelloLocal" />
```

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

Add your LocalLiveView modules to `local/lib/`. They will be compiled into the WASM bundle when you run `mix llv.build`.

## Security headers

LocalLiveView requires [SharedArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer), which browsers only allow with the following HTTP headers set:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

The installer adds a `put_wasm_security_headers/2` plug to your endpoint automatically. If you use a CDN or reverse proxy, make sure these headers are forwarded.
