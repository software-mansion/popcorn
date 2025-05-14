[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/dark-mode-logo.svg">
  <source media="(prefers-color-scheme: light)" srcset="assets/light-mode-logo.svg">
  <img alt="Popcorn" src="assets/fallback-logo.svg">
</picture>

**Popcorn is a library that allows you to run client-side Elixir in browsers, with Javascript interoperability**

## Usage

Add the project to dependencies:

```elixir
# mix.exs

def deps do
  {:popcorn, github: "software-mansion/popcorn"}
end
```

and to compilers:

```elixir
# mix.exs

def project do
  [
    # ...
    compilers: Mix.compilers() ++ [:popcorn]
  ]

end
```

Create a startup module:

```elixir
# lib/my_app/start.ex

defmodule MyApp.Start do
  def start() do
    Console.print("Hello World\n")
  end
end
```

and register it in the config:

```elixir
# config/config.exs

config :popcorn, start_module: MyApp.Start
```

Finally, run `mix deps.get` and `mix compile`, and the `bundle.avm` file will be generated in the `_build/dev/` directory. You can run it with `atomvm bundle.avm`.

Alternatively, instead of adding `:popcorn` to compilers, you can run `Popcorn.cook/1` after `mix compile` with the same effect.

## Configuration options

These options can be set by putting a `config :popcorn, option: value` line in `config/config.exs`:

- `start_module` - The `start/0` function in this module will be the entry point your app. Defaults to `nil` - no function will be called, the output should be embedded in another `.avm` file.
- `out_dir` - The path to the output `.avm` file. Defaults to `path_to_build/bundle.avm`, where `path_to_build` is the output of `Mix.Project.build_path/0`.
- `add_tracing` - If `true`, injects a simple tracing code that prints module, function and arity of each cross-module call. Defaults to `false`.
- `erl_stdlib_beam_paths` - List of paths to Erlang stdlib `.beam` files. Defaults to `Path.wildcard("#{:code.lib_dir()}/{compiler,erts,kernel,stdlib}*/**/*.beam")`
- `ex_stdlib_beam_paths` - List of paths to Elixir stdlib `.beam` files. Defaults to `Path.wildcard("#{Application.app_dir(:elixir)}/ebin/**/*.beam")`
- `runtime_source` - See `Mix.Tasks.Popcorn.BuildAvm` or type `mix help popcorn.build_runtime`

## Authors

Popcorn is created by Software Mansion.

Since 2012 [Software Mansion](https://swmansion.com/) is a software agency with experience in building web and mobile apps as well as complex multimedia solutions. We are Core React Native Contributors and experts in live streaming and broadcasting technologies. We can help you build your next dream product – [Hire us](https://swmansion.com/contact/projects).

Copyright 2025, [Software Mansion](https://swmansion.com/)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/)

Licensed under the [Apache License, Version 2.0](LICENSE)
