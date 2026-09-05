# Work on the Popcorn runtime

This page describes repository development. Application users do not need to
build the BEAM virtual machine.

## Repository areas

- `popcorn/patches/` contains Popcorn changes to Erlang/OTP.
- `popcorn/sources/` contains generated source checkouts.
- `popcorn/out/` contains virtual machine build output.
- `popcorn/js/` contains the browser runtime, bridge, and bundler plugins.
- `popcorn/elixir/` contains the Elixir bridge API.
- `examples/` contains browser applications.

Generated source and output directories are not release source files.

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
