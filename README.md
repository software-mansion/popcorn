[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/dark-mode-logo.svg">
  <source media="(prefers-color-scheme: light)" srcset="assets/light-mode-logo.svg">
  <img alt="Popcorn" src="assets/fallback-logo.svg">
</picture>

**Popcorn is a library that allows you to run client-side Elixir in browsers, with Javascript interoperability**

Popcorn is early stages and may break. Please report an issue if it does. Contributions are very welcome, but please open an issue before commiting too much effort.

Under the hood, Popcorn runs [AtomVM](https://github.com/atomvm/AtomVM), a tiny Erlang VM.

## Examples

The examples are hosted at [popcorn.swmansion.com](https://popcorn.swmansion.com), and the source code is in the `examples` directory.

## Getting started

*Note: Popcorn currently only works OTP 26.0.2 and Elixir 1.17.3. We're working to lift this requirement.*

Popcorn requires just a few short steps to setup. Firstly, add the project to dependencies:

```elixir
# mix.exs

def deps do
  {:popcorn, github: "software-mansion/popcorn"}
end
```

Create a startup module:

```elixir
# lib/my_app/start.ex

defmodule MyApp.Start do
  def start() do
    Popcorn.Wasm.register("main")
    IO.puts("Hello from WASM")
  end
end
```

and register it in the config, along with the directory to output static artifacts:

```elixir
# config/config.exs
import Config
config :popcorn, start_module: MyApp.Start, out_dir: "static/wasm"
```

Run `mix deps.get` and `mix popcorn.cook`. The latter will generate WASM artifacts in the `static/wasm` directory. Add a simple HTML file that will load it:

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

The easiest way to host the page is to generate a simple HTTP server script with `mix popcorn.simple_server` and run it with `elixir server.exs`. Then, at http://localhost:4000, you should see `Hello from WASM` printed in the console.

The webpage can also be hosted with any HTTP static file server, but it must add the following HTTP headers:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Otherwise, browsers refuse to run WASM.

## Authors

Popcorn is created by Software Mansion.

Since 2012 [Software Mansion](https://swmansion.com/) is a software agency with experience in building web and mobile apps as well as complex multimedia solutions. We are Core React Native Contributors and experts in live streaming and broadcasting technologies. We can help you build your next dream product – [Hire us](https://swmansion.com/contact/projects).

Copyright 2025, [Software Mansion](https://swmansion.com/)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/)

Licensed under the [Apache License, Version 2.0](LICENSE)
