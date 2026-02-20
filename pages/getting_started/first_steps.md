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

A minimal worker can optionally register itself using `Popcorn.Wasm.register_default_receiver/2`:

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
    Popcorn.Wasm.register_default_receiver(self(), @process_name)
    IO.puts("Hello from WASM!")
    state = %{}
    {:ok, state}
  end
end
```

Then, configure the directory to output static artifacts:

```elixir
# config/config.exs
import Config
config :popcorn, out_dir: "static/wasm"
```

Finally, run `mix deps.get` and `mix popcorn.cook`. The latter will generate WASM artifacts in the `static/wasm` directory. Add a simple HTML file that will load it:

```html
<!-- index.html -->
<html>
  <script type="module">
    import { Popcorn } from "./wasm/popcorn.js";
    await Popcorn.init({ onStdout: console.log });
  </script>
  <body></body>
</html>
```

### Serving the artifacts

The easiest way to host the page is to generate a simple HTTP server script with `mix popcorn.simple_server` and run it with `elixir server.exs`. Then, at <http://localhost:4000>, you should see `Hello from WASM` printed in the console.

The webpage can also be hosted with any HTTP static file server, but **it must add the following HTTP headers**:

```headers
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Otherwise, browsers refuse to run WASM.

### Live reloading

Popcorn also ships with a server that supports live reloading. It can be generated with `mix popcorn.dev_server` and run with `elixir dev_server.exs`.

The index.html requires an extra line to load the required JavaScript.

```html
<!-- index.html -->
<html>
  <script type="text/javascript" src="/dev_server.js"></script>
  <script type="module">
    import { Popcorn } from "./wasm/popcorn.js";
    await Popcorn.init({ onStdout: console.log });
  </script>
  <body></body>
</html>
```

## Popcorn project files structure

Here's the breakdown of files served in the Popcorn's based project

```console
your-app
└── static
    ├── index.html             HTML skeleton loading Popcorn scripts
    └── wasm
        ├── AtomVM.mjs         Runtime glue code for browser APIs
        ├── AtomVM.wasm        Compiled AtomVM runtime
        ├── bundle.avm         Bundled Elixir bytecode
        ├── popcorn_iframe.js  Manages Wasm module, internal
        └── popcorn.js         Initializes Popcorn, allows Elixir communication
```
