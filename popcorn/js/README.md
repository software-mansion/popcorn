# Popcorn OTP

Run Elixir in a browser with OTP/BEAM compiled to WebAssembly.
This prerelease replaces the AtomVM runtime used by Popcorn 0.3.x.

## Install

```sh
npm install @swmansion/popcorn@next
```

Add `{:popcorn, "0.4.0-next.0"}` to your Elixir application's dependencies.
Run `mix deps.get` and `mix compile` before building the JavaScript application.
The bundler plugins invoke Mix locally to package application and standard-library code.

Use the toolchain pinned in [popcorn/mise.toml](https://github.com/software-mansion/popcorn/blob/v0.4.0-next.0/popcorn/mise.toml).
The packager checks host OTP compatibility against the selected runtime's manifest.

## Configure Vite

```ts
import { defineConfig } from "vite";
import { popcorn } from "@swmansion/popcorn/vite";

export default defineConfig({
  plugins: [
    popcorn({
      rootDir: "../",
      app: "my_app",
    }),
  ],
});
```

Set `rootDir` to the compiled Mix project's directory and `app` to its OTP application name.
Use `app: null` to package the base runtime without starting an application.

The npm package contains two variants:

- `core`: without native crypto support.
- `crypto`: includes native crypto and ASN.1 support for applications that depend on crypto, public_key, or ssl.

The plugin emits only the selected variant. The browser does not download both.
Both variants share one JavaScript API and one Hex package.
The plugin selects `crypto` when the application's dependencies or `extraApps` require it; otherwise it selects `core`.
Set `runtimeVariant: "core"` or `runtimeVariant: "crypto"` to override this choice.
An explicit `"core"` selection produces a build error if the application requires crypto.

Rollup and esbuild plugins accept the same options through `@swmansion/popcorn/rollup` and `@swmansion/popcorn/esbuild`.
Use ESM output with those bundlers.

## Start the runtime

```ts
import { Popcorn } from "@swmansion/popcorn";

const result = await Popcorn.init({});
if (!result.ok) throw result.error;

const vm = result.data;
// Stop the runtime when the application no longer needs it.
vm.deinit();
```

## Serve in production

Serve over HTTPS or localhost. Set these headers on the application and runtime responses:

```text
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Vite sets these headers for development and preview. Configure your production server separately.
Serve `.wasm` as `application/wasm`. Serve compressed `.tar.gz` files with `Content-Encoding: gzip` for `.tar` requests.
The package also emits uncompressed tar files. Brotli variants require `brotli: true` and the corresponding server configuration.

The JavaScript bridge currently requires a Content Security Policy that permits `unsafe-eval`.
See the [versioned Elixir API](https://popcorn.hexdocs.pm/0.4.0-next.0/) for interoperability details.
