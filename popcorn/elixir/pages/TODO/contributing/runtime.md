# Work on the Popcorn runtime

This page describes repository development. Application users do not need to
build the BEAM virtual machine.

## Repository areas

- `popcorn/patches/` contains Popcorn changes to Erlang/OTP.
- `popcorn/sources/` contains generated source checkouts.
- `popcorn/out/js/` contains generated JavaScript, declarations, and bundler tools.
- `popcorn/out/runtimes/` contains the built virtual machine variants.
- `popcorn/js/` contains the browser runtime, bridge, and bundler plugins.
- `popcorn/elixir/` contains the Elixir bridge API.
- `examples/` contains browser applications.

Generated source and output directories are not release source files.

The JS build replaces `out/js/` and links its `runtimes/` directory to
`out/runtimes/`. Local `js/dist/` and `elixir/priv/static/` link to `out/js/`.
The npm and Hex packages copy this tree with symlinks resolved. npm packaging
requires both the core and crypto variants; Hex includes the variants built
locally. Bundler plugins generate application boot files, tarballs, and manifests
separately.

## Use the pinned toolchain

Install the root tools:

```console
mise install
pnpm install
```

Build a debug version of the core OTP runtime:

```console
mise run build-otp debug
```

Build the crypto runtime when a change affects native crypto or ASN.1 support:

```console
mise run build-otp-with-crypto debug
```

## Test a change

Run the narrowest relevant test first. Then run the complete suite for the
changed package.

```console
mise run test --js
mise run test --wasm
```

Browser tests need the runtime assets and a Chromium installation.

## Change patched OTP source

Do not edit generated OTP source without updating its named patch. Use the
repository patch script to regenerate patches after a source change:

```console
mise run regenerate-otp-patches
```

Review the generated patch before you keep it.
