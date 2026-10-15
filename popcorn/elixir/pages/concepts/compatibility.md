# Compatibility and limits

Popcorn uses OTP/BEAM directly which gives a lot of things for free. However, some things can't work with the browser's sandbox model or need to be worked around.

## Compatibility

Popcorn uses fairly new versions of OTP/BEAM but uses applications compiled by the local toolchain. Standard library `.beam`s are taken from your computer but they may depend on VM built-in modules or NIFs.

Our NIF dependency detection code can catch some incompatibilities caused by a mismatched installation but it's important to know that your standard library code may depend on VM features that were removed in later versions or use new NIFs not available in previous ones.

## Limits

### Dynamic NIF loading

Unsupported. All NIFs are statically linked to the VM at compilation time. This includes crypto, which is selected automatically and can be overridden by setting `config :popcorn, runtime_variant: :crypto` (or `:core`) in `config/config.exs`.

### `SharedArrayBuffer` and JavaScript evaluation

The shared buffer is required for Popcorn to function. We also evaluate JavaScript as strings when running `Popcorn.JS.run/3`.

You need to have the following headers set:

```text
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
Content-Security-Policy: script-src 'unsafe-eval'
```

### TCP/UDP sockets

Browsers don't allow applications to create raw sockets so `:gen_tcp` and `:gen_udp` won't work.

We're tracking alternatives in https://github.com/software-mansion/popcorn/issues/789.

### Spawning OS processes

Popcorn doesn't support spawning OS processes. The VM machinery for starting those processes has been removed. BEAM processes are supported.

### Distribution

Connecting to other nodes is currently unsupported. We may offer some replacement in the future (cross-tab or via WebSockets). Erlang distribution doesn't have security features and you can only connect to nodes you trust.

## Memory

Each VM starts with 64 MiB of shared Wasm linear memory. It can grow to 256 MiB on phones and tablets, or 2 GiB on desktop. We use heuristics to detect the type of device.

Configure the limits for an application that needs a different budget:

```typescript
const popcorn = await Popcorn.init({
  beam: { maxMemory: { mobile: "512M", desktop: "1G" } },
});
```

Omit `maxMemory` to keep the defaults. Each key can be overridden separately.
A browser can refuse an allocation below the configured limit and can refuse to load the `.wasm` file if it requests too much memory (even when the application doesn't use that much).
