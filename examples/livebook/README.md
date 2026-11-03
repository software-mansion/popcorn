# Livebook in Popcorn (spike)

[Livebook](https://livebook.dev) 0.19, unmodified from Hex, running in the
browser's VM. It runs as a distributed node, and notebooks evaluate in its
embedded runtime, in the same VM. Nothing goes over a network: requests and
WebSocket frames go between the page and the VM.

## Running

It needs Popcorn's runtime built from this branch, with distribution
(`scripts/build-beam.sh --with-crypto release`, then `pnpm build` in `popcorn/js`).

```sh
mix deps.get
mix popcorn.dev
```

Open [localhost:4000](http://localhost:4000). `node scripts/check.mjs` opens
Livebook in headless Chromium, creates a notebook and evaluates cells.

## How it works

- `public/index.html` is a shell page that owns the VM, so the VM outlives
  Livebook's navigations. Livebook runs in an iframe.
- `public/sw.js`, a Service Worker, sends the iframe's navigations, form posts
  and fetches to the shell, which calls `Popcorn.Plug` in the VM. Livebook's
  static files load from the network: `mix popcorn.dev` copies them to `public/`,
  and they're left out of the VM's package.
- Service Workers can't intercept WebSockets, so the Service Worker adds
  `public/popcorn-ws.js` to Livebook's pages, which replaces `WebSocket` with one
  whose other end is a socket process in the VM.
- Responses built in JavaScript can't set cookies, so the shell keeps them.
- `Popcorn.Plug` serves the requests with `LivebookWeb.Endpoint`, like the
  Phoenix example.
- `LivebookWasm.Application` starts Livebook after its stand-ins, and
  `Popcorn.Plug` after Livebook: memsup, which Livebook calls for memory usage,
  doesn't run in the browser.
- `config/config.exs` has the settings Livebook's config files and
  `Livebook.config_runtime/0` would make, as Popcorn evaluates only this file.

## What doesn't work

- Outputs rendered in iframes, like Kino's JavaScript outputs, which Livebook
  serves from a second port.
- `Mix.install/2` and other runtimes than the embedded one.
- Persistence: Livebook's files are in the VM's memory, so a reload loses them.
- Evaluating right after typing can evaluate the cell's previous source; the
  check waits half a second after typing.
