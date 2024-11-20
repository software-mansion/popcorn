# FissionLib

Library that takes Erlang and Elixir stdlibs from the system, patches them so that they can work with AtomVM, and bundles them along with your project into an `.avm` file that can be run with AtomVM.

The idea is the following:
- Decouple AtomVM versions of Erlang and Elixir stdlibs from the AtomVM itself
- Don't transfer any code from the original stdlibs into the AtomVM versions anymore
- Instead, take the stdlibs from the system
- For the stdlib code that doesn't work in AtomVM (usually because of missing NIFs),
provide a custom implementations in the FissionLib
  - For the start, it's all the code from the AtomVM versions of stdlibs.
  - (TBD) In the future, the parts of them that are copied from original stdlibs should be
  indentified and removed
  - (In progress) Subsequently identify missing functions in the AtomVM and either add missing
  NIFs there or provide custom implementations in the FissionLib
- Then, patch the original stdlibs with custom implementations by replacing particular functions
- Provide a convenient way of bundling projects along with the patched stdlibs into a single `.avm` file (see [Usage](#usage))
- (TBD) prune out the code that's not used by a given project to reduce the size of the bundle

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

Alternatively, instead of adding `:fission_lib` to compilers, you can run `FissionLib.pack/1` after `mix compile` with the same effect.

## Configuration options

These options can be set by putting a `config :fission_lib, option: value` line in `config/config.exs`:

- `start_module` - The `start/0` function in this module will be the entry point your app. Defaults to `nil` - no function will be called, the output should be embedded in another `.avm` file.
- `out_path` - The path to the output `.avm` file. Defaults to `path_to_build/bundle.avm`, where `path_to_build` is the output of `Mix.Project.build_path/0`.
- `add_tracing` - If `true`, injects a simple tracing code that prints module, function and arity of each cross-module call. Defaults to `false`.
- `erl_stdlib_beam_paths` - List of paths to Erlang stdlib `.beam` files. Defaults to `Path.wildcard("#{:code.lib_dir()}/{compiler,erts,kernel,stdlib}*/**/*.beam")`
- `ex_stdlib_beam_paths` - List of paths to Elixir stdlib `.beam` files. Defaults to `Path.wildcard("#{Application.app_dir(:elixir)}/ebin/**/*.beam")`
