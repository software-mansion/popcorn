# Troubleshooting

Start with the first visible error. Match JavaScript errors by `error.t`.

## The virtual machine does not start

**Symptoms:** `worker:load`, `beam:missing-boot-script`,
`beam:missing-manifest`, or `beam:missing-tarball`.

Check the browser network panel. Confirm that the generated worker, WebAssembly
file, boot file, manifest, and application archives return successful responses.

If you set `beam.otpAssetsRoot`, confirm that the value ends with `/`.

## The browser reports a SharedArrayBuffer error

Set these headers on the page and runtime responses:

```http
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Check the final production origin. A development server can hide a missing
production configuration.

## Popcorn returns `runtime:eval-unavailable`

The page Content Security Policy blocks JavaScript evaluation. The current
`Popcorn.Wasm.run_js/3` bridge needs `unsafe-eval`.

Update the policy only after you review its security effect.

## A GenServer call returns `genserver:noproc`

Confirm that the target has a registered name. Confirm that the application
supervises both the target and `Popcorn.Proxy`.

Use the same name in JavaScript and Elixir. JavaScript uses a string such as
`"counter"` for the Elixir name `:counter`.

## A message returns `bridge:unserializable`

Check the value conversion table in [Values across the bridge](values.html).

Remove functions, class instances, cycles, bigints, unsafe integers, and
non-finite numbers. Convert application data to a small plain object first.

## A call times out

Check whether the GenServer performs long work in a callback. A timeout does
not cancel work in the target process. Set `timeoutMs` in the call options when
the operation legitimately needs more time.

Also check for a bridge deadlock. JavaScript called by `run_js/3` must not call
the GenServer that waits for that JavaScript result.

## Development works but production fails

Check HTTPS, COOP, COEP, MIME types, compressed response headers, and generated
asset paths. See [Deploy Popcorn](deployment.html).
