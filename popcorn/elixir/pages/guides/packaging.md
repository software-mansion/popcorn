# Package an application

`mix popcorn.cook` compiles the Mix project and packages its build output. For
JavaScript-based projects, the Vite, Rollup, and esbuild plugins produce the
same package.

```bash
mix popcorn.cook --out-dir priv/static/popcorn
```

The result looks like this:

```
priv/static/popcorn/
├── index.mjs
├── worker.mjs
├── beam.mjs
├── beam.emu.mjs
├── beam.wasm
└── otp/
    ├── manifest.json
    ├── bin/
    │   └── vm.boot
    └── lib/
        ├── my_app.tar
        ├── my_app.tar.gz
        ├── my_app.tar.br
        └── ...
```

The exact `.tar` archives depend on the selected application and its dependencies.

## Browser runtime files

The files at the output root run the BEAM virtual machine in the browser:

- `index.mjs` exports the Popcorn JavaScript API.
- `worker.mjs` runs the virtual machine outside the browser's main thread.
- `beam.mjs` loads the Emscripten runtime and `beam.wasm`.
- `beam.emu.mjs` runs the Emscripten pthread worker.
- `beam.wasm` contains the BEAM virtual machine.

The output includes the browser API and can be loaded without a JavaScript
bundler:

```html
<script type="module">
  import { Popcorn } from "/popcorn/index.mjs";

  const result = await Popcorn.init();
  if (!result.ok) throw result.error;
</script>
```

Keep these files in the generated layout. The modules resolve workers and
WebAssembly files relative to their own URLs.

## OTP package files

The `otp/` directory contains the files loaded into the virtual machine:

- `manifest.json` describes the runtime, entrypoint, and application archives.
- `bin/vm.boot` contains the boot script for the packaged OTP release.
- `lib/<app>.tar` contains one application's compiled `ebin` directory.
- `lib/<app>.tar.gz` and `lib/<app>.tar.br` are precompressed copies of that
  archive.

Each application archive expands to `lib/<app>/ebin`. It contains compiled
BEAM modules and the application's `.app` resource file. The package does not
include source files, `priv` directories, or Mix runtime configuration.

The browser requests the uncompressed `.tar` URL. Configure the server to
select its `.tar.gz` or `.tar.br` sibling through HTTP content negotiation.
See [Deploy Popcorn](deployment.html) for the required response headers.

### Package manifest

`otp/manifest.json` is the inventory used during boot. Its main fields are:

- `entrypoint` names the application started after the virtual machine boots.
  It is `null` when no application should start.
- `runtimeVariant` identifies the included `core` or `crypto` runtime.
- `apps` maps every packaged application to its archive and version.
- `vm` identifies the boot file and the OTP runtime version.
- `toolchain` records the OTP and Elixir versions used for packaging.
- `notes` reports compatibility issues, such as modules that load dynamic
  native implemented functions (NIFs).

Archive and boot paths in the manifest are relative to the `otp/` directory.
Do not rename these files without also updating the manifest.

## Select the entrypoint

Set the application in the bundler configuration:

```typescript
popcorn({
  rootDir: "../",
  app: "my_app",
});
```

The task uses the active Mix environment and target build path. By default it
packages and starts the current Mix application. Set `app` to select another
application explicitly.

The packager includes the entrypoint and its required application dependencies.
Set `app: null` to start no application.

Dependency selection follows the `applications` and `included_applications`
entries in each `.app` file. Optional applications are not included unless you
add them explicitly. The Elixir runtime and its required applications are
always included, even when `app` is `null`.

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

The `strip` option removes nonessential BEAM chunks. It defaults to `true`.

The `treeshake` option removes unreachable modules and functions. It is
experimental and disabled by default.

The packager always adds Brotli tar variants. Standard effort uses quality 9.

Use `brotliEffort: "max"` in a bundler plugin or `--brotli-effort max` with
`mix popcorn.cook` to use quality 11 for a release build.
