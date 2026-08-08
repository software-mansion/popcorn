# IEx in WebAssembly

Runs the real Elixir IEx user driver on Popcorn's OTP/BEAM WebAssembly runtime
and renders it with [`ghostty-web`](https://github.com/coder/ghostty-web).

There is no Elixir project here. `assets/build.mjs` names `:iex` as the
entrypoint and `:logger` as an extra app, so both are packed straight out of
the local Elixir installation. The shell is started through emulator arguments
that mirror the native `iex` launcher:

```text
-elixir_root /lib -user elixir -extra --no-halt +iex --dot-iex ""
```

A local Erlang/Elixir install is still required, since that is where the packed
applications come from.

Install the JavaScript dependencies and build the OTP package from the
repository root:

```bash
pnpm install
mise run build-otp-js
```

Then build and serve the example from `assets/`:

```bash
pnpm dev
```

Open [localhost:5173](http://localhost:5173).

The browser console exposes `globalThis.iexWasm` with the Popcorn runtime and
the Ghostty terminal, for poking at the session by hand.

This example also contains [Playwright](https://playwright.dev)-based tests. To
run them from the `assets` directory, type:

```bash
pnpm exec playwright install chromium
pnpm test
```
