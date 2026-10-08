# Package an application

`mix popcorn.cook` compiles the Mix project and packages its build output. The
Vite, Rollup, and esbuild plugins invoke the same task.

```console
mix popcorn.cook --out-dir public/out
```

Keep the output outside `priv`: Mix packs `priv` into the application archive, so
the assets would ship twice.

The output includes the browser API and can be loaded without a JavaScript
bundler:

```html
<script type="module">
  import { Popcorn } from "/out/index.mjs";

  const result = await Popcorn.init();
  if (!result.ok) throw result.error;
</script>
```

## Develop locally

Run the development task from your Mix project:

```console
mix popcorn.dev
```

It cooks the application, serves `public` at `http://localhost:4000` with
COOP and COEP headers, and cooks again when source files change. Open your
`public/index.html` page and refresh the browser after a successful cook.
Project configuration, `priv` files, and local path dependency sources are
watched through native filesystem events using `file_system`. Generated cook
output is excluded. If a rebuild fails, fix the source
and the next edit triggers another cook.

The task accepts every `popcorn.cook` option, plus `--dir` and `--port`:

```console
mix popcorn.dev --port 8080
```

The server installs Bandit and Plug in a separate process on its first run,
which requires network access. They are not added to your application's
dependencies. On Linux, install `inotify-tools` for the file watcher.

## Select the entrypoint

Set the application in the bundler configuration:

```typescript
popcorn({
  rootDir: "../",
});
```

The task uses the active Mix environment and target build path. By default it
packages and starts the current Mix application. Set `app` to select another
application explicitly.

The packager includes the entrypoint and its required application dependencies.
Set `app: null` to start no application. This option does not package every
application in the build directory.

## Add optional applications

Use `extraApps` for optional or dynamically loaded applications:

```typescript
popcorn({
  rootDir: "../",
  extraApps: ["eex"],
});
```

The packager includes each extra application's required dependencies. It does
not start the extra application.

## Select a runtime variant

Popcorn provides two runtime variants:

- `core` excludes native crypto and ASN.1 support.
- `crypto` includes support required by `crypto`, `public_key`, and `ssl`.

The plugin selects `crypto` when packaged applications require it. Otherwise,
it selects `core`.

Set `runtimeVariant` only when you need an explicit choice. The build fails if
an explicit `core` choice conflicts with application requirements.

## Control output size

The `strip` option removes nonessential BEAM chunks. It defaults to `true` and
remains experimental.

The `treeshake` option removes unreachable modules and functions. It is
disabled by default. Set it to an object to enable it, and use
`preservedApps` to keep every module in selected applications.

The packager always adds Brotli tar variants. Standard effort uses quality 9.
Use `brotliEffort: "max"` in a bundler plugin or
`--brotli-effort max` with `mix popcorn.cook` to use quality 11 for a release
build.

Application packaging includes each selected application's `ebin` directory and `priv` directory when present.
