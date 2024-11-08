# FissionLib

Library providing Erlang and Elixir stdlibs, allowing to compile projects to `.avm` and run them with AtomVM.

## Usage

Add the project to dependencies:

```elixir
# mix.exs

def deps do
  {:fission_lib, github: "software-mansion-labs/elixir-wasm", sparse: "fission_lib"}
end
```

and to compilers:

```elixir
# mix.exs

def project do
  [
    # ...
    compilers: Mix.compilers() ++ [:fission_lib]
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

config :fission_lib, start_module: MyApp.Start
```

Finally, run `mix deps.get` and `mix compile`, and the `bundle.avm` file will be generated in the `_build/dev/` directory. You can run it with `atomvm bundle.avm`.

## Configuration options

- `start_module` - the `start/0` function in this module will be the entry point your app. Defaults to `nil` - no function will be called, the output should be embedded in another `.avm` file.
- `out_path` - the path to the output `.avm` file
- `add_tracing` - if true, injects a simple tracing code that prints module, function and arity of each cross-module call
- `erl_stdlib_beam_paths` - list of paths to Erlang stdlib `.beam` files
