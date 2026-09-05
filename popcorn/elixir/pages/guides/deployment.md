# Deploy Popcorn

A Popcorn deployment must serve the page, worker, WebAssembly file, boot file,
manifest, and application archives.

## Use a secure context

Serve the application over HTTPS. Browsers also accept `localhost` during
development.

## Set cross-origin isolation headers

Set these headers on the page and runtime responses:

```http
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

The Vite plugin sets them for development and preview. Configure the production
server separately.

## Set response metadata

Serve `.wasm` files as `application/wasm`.

When the server selects a gzip archive, set `Content-Encoding: gzip`. When it
selects a Brotli archive, set `Content-Encoding: br`.

The `Content-Encoding` header must match the compressed asset.

## Keep generated paths intact

Keep the generated worker, runtime files, and `otp/` directory at their output
locations. Web Workers resolve related runtime files by URL, so these locations
must remain stable.

Use `beam.otpAssetsRoot` only for custom hosting. The value must end with `/`.

## Configure the Content Security Policy

`Popcorn.Wasm.run_js/3` currently evaluates JavaScript source. The page Content
Security Policy must permit `unsafe-eval`.

`Popcorn.init()` returns `runtime:eval-unavailable` when the page blocks this
operation.

## Check the production build

1. Load the application from its final HTTPS origin.
2. Confirm that the worker and WebAssembly requests succeed.
3. Confirm that the boot file, manifest, and archives return successful responses.
4. Confirm that the browser reports cross-origin isolation.
5. Exercise one JavaScript-to-BEAM call and one BEAM-to-JavaScript event.
