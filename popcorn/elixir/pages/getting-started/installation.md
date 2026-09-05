# Installation

This guide installs the Popcorn 0.4 prerelease in an existing Mix application.

## Requirements

Use Elixir 1.19 or later. Use the OTP version from the selected Popcorn release
toolchain.

You also need Node.js, a JavaScript package manager, and a supported bundler.
Popcorn provides plugins for Vite, Rollup, and esbuild.

## Add the packages

Add Popcorn to `mix.exs`:

```elixir
defp deps do
  [
    {:popcorn, "0.4.0-next.0"}
  ]
end
```

Get the dependency and compile the application:

```console
mix deps.get
mix compile
```

Install the matching JavaScript package:

```console
npm install @swmansion/popcorn@next
```

The Elixir and JavaScript package versions must match.

## Configure Vite

Add the Popcorn plugin to `vite.config.ts`:

```typescript
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

Set `rootDir` to the Mix project directory. Set `app` to the OTP application
name from `mix.exs`.

The plugin packages compiled BEAM files. Run `mix compile` before the JavaScript
build.

## Start the runtime

Start Popcorn from the JavaScript entry point:

```typescript
import { Popcorn } from "@swmansion/popcorn";

const result = await Popcorn.init();
if (!result.ok) throw result.error;

const popcorn = result.data;
```

Vite supplies the required development headers. Your production server must
supply them too. See [Deploy Popcorn](deployment.html).

Continue with [Build your first Popcorn application](first-application.html).
