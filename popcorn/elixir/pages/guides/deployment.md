# Deploy Popcorn

A Popcorn deployment must serve the VM and tarballs with compiled apps.

## Set cross-origin isolation headers

Set these headers on the page and runtime responses:

```http
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

These headers are required for [`SharedArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer) to work.

The Vite plugin and `mix popcorn.dev` sets them for development and preview. Configure the production
server separately.

## Set response metadata

Serve `.wasm` files as `application/wasm`.

When the server selects a gzip archive, set `Content-Encoding: gzip`. When it
selects a Brotli archive, set `Content-Encoding: br`.

The `Content-Encoding` header must match the compressed asset. Popcorn by default generates uncompressed, gzip, and brotli assets. You can [configure Brotli effort](TODO) for slightly smaller assets.

## Keep generated paths intact

Make sure that output layout is unmodified. Popcorn uses WebWorkers which need a stable location for scripts they run.

Use `beam.otpAssetsRoot` if you want to run it on subpage. The value must end with `/`.

## Configure the Content Security Policy

`Popcorn.Wasm.run_js/3` currently evaluates JavaScript source. The page Content
Security Policy must permit `unsafe-eval` — Popcorn initialization will fail if it's not allowed.
