# First steps

> ### Compatibility warning {: .warning}
>
> Popcorn currently only works OTP 26.0.2 and Elixir 1.17.3. We're working to lift this requirement.

Popcorn requires just a few short steps to setup. Firstly, add the project to dependencies and ensure you have an application start callback:

```elixir
# mix.exs

def application do
  [
    extra_applications: [],
    mod: {MyApp.Application, []}
  ]
end

def deps do
  [
    {:popcorn, "~> 0.1.0"}
  ]
end
```

The application should start a worker process:

```elixir
# lib/my_app/application.ex
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      MyApp.Worker
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

After initialization is complete, we must call `Popcorn.Wasm.ready/1`. `Popcorn.init()` on the JS side waits until this is called before resolving. Passing a process name registers it as the default receiver for JS `call`/`cast`:

```elixir
# lib/my_app/worker.ex

defmodule MyApp.Worker do
  use GenServer

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    Popcorn.Wasm.ready(@process_name)
    IO.puts("Hello from WASM!")
    {:ok, %{}}
  end
end
```

Then, configure the output directory for compiled artifacts:

```elixir
# config/config.exs
import Config
config :popcorn, out_dir: "dist/wasm"
```

Next, fetch dependencies and compile your Elixir code into a Popcorn `.avm` bundle:

```console
$ mix deps.get
$ mix popcorn.cook
```

### Setting up the JavaScript build

Popcorn uses a JavaScript bundler to package the runtime and your compiled code for the browser. A generator scaffolds the required files:

```console
$ mix popcorn.gen.js
```

This creates an `assets/` directory with:

- `package.json` — npm project with [`@swmansion/popcorn`](https://www.npmjs.com/package/@swmansion/popcorn) and `esbuild`
- `build.mjs` — esbuild configuration with the Popcorn plugin
- `index.js` — JavaScript entry point that initializes Popcorn
- `index.html` — HTML template

The generated `index.js` looks like this:

```javascript
// assets/index.js
import { Popcorn } from "@swmansion/popcorn";

await Popcorn.init({ bundlePath: "/wasm/bundle.avm", onStdout: console.log });
```

`bundlePath` tells Popcorn where to find the compiled Elixir bytecode, matching the `out_dir` you configured earlier. `onStdout` receives anything your Elixir code prints with `IO.puts/1`.

Install the npm dependencies and build:

```console
$ npm install --prefix assets
$ npm run build --prefix assets
```

The JavaScript build step bundles your JavaScript, copies the WebAssembly runtime assets, and outputs everything to the `dist/` directory. `mix popcorn.cook` only produces the `.avm` bundle.

> #### Other bundlers {: .info}
>
> The generator uses esbuild, but Popcorn also ships plugins for Vite (`@swmansion/popcorn/vite`) and Rollup (`@swmansion/popcorn/rollup`). You can swap the bundler by editing `build.mjs` after scaffolding.
>
> Vite is especially convenient for development — its dev server provides hot reloading out of the box. See the [hello-react example](https://github.com/software-mansion/popcorn/tree/main/examples/hello-react) for a working Vite setup.

### Serving the artifacts

Run `mix popcorn.server` to start a local server. Then, at <http://localhost:4000>, you should see `Hello from WASM` printed in the browser console.

The webpage can also be hosted with any HTTP static file server, but **it must add the following HTTP headers**:

```headers
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Otherwise, browsers refuse to run WASM.

## Popcorn project files structure

Here's the breakdown of a Popcorn project:

```console
your-app
├── assets                  Source files (JavaScript, HTML)
│   ├── build.mjs           esbuild config with Popcorn plugin
│   ├── index.html          HTML template
│   ├── index.js            JS entry point — initializes Popcorn
│   └── package.json        npm dependencies
├── config
│   └── config.exs          Popcorn configuration (out_dir)
├── dist                    Build output (generated)
│   ├── AtomVM.mjs          Runtime glue code for browser APIs
│   ├── AtomVM.wasm         Compiled AtomVM runtime
│   ├── index.html          Copied HTML
│   ├── index.js            Bundled JavaScript
│   └── wasm
│       └── bundle.avm      Bundled Elixir bytecode
└── lib
    └── my_app
        ├── application.ex  Application supervisor
        └── worker.ex       Worker process
```
