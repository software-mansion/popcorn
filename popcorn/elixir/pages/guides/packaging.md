# Package an application

`mix popcorn.cook` packages existing Mix build output. The Vite, Rollup, and
esbuild plugins invoke the same task. Packaging does not replace `mix compile`.

```console
mix popcorn.cook --out-dir priv/static/popcorn
```

The output includes the browser API and can be loaded without a JavaScript
bundler:

```html
<script type="module">
  import { Popcorn } from "/popcorn/index.mjs";

  const result = await Popcorn.init();
  if (!result.ok) throw result.error;
</script>
```

## Select the entrypoint

Set the application in the bundler configuration:

```typescript
popcorn({
  rootDir: "../",
  app: "my_app",
});
```

The task uses the active Mix environment and target build path.

The packager includes the entrypoint and its required application dependencies.
Set `app: null` to start no application. This option does not package every
application in the build directory.

## Add optional applications

Use `extraApps` for optional or dynamically loaded applications:

```typescript
popcorn({
  rootDir: "../",
  app: "my_app",
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

The `brotli` option adds Brotli tar variants. It defaults to `false`.

Application packaging currently includes `ebin` directories. It does not copy
`priv` files or Mix runtime configuration.
