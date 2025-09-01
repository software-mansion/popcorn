# Changing the used runtime

Popcorn runs AtomVM under the hood, and therefore it needs to either download
precompiled artifacts or compile it from source.
The default configuration should work out of the box,
but you can change it to use a different version of AtomVM.

Precompiled artifacts are downloaded during Popcorn compilation,
from the source specified with `runtime` config key, for example:

```elixir
config :popcorn, runtime: [
  {:url, "https://atomvm/wasm/url", target: :wasm},
  {:path, "/path/to/atomvm/unix", target: :unix}
]
```

If you want to build the runtime from source, run
`mix popcorn.build_runtime --target <unix|wasm>`.
Make sure you have [AtomVM dependencies](https://github.com/atomvm/atomvm?tab=readme-ov-file#dependencies) installed.
Then, configure the runtime as follows:

```elixir
config :popcorn, runtime: {:path, "popcorn_runtime_source/artifacts/#{target}", target: target}
```

See `mix popcorn.build_runtime` docs for more configuration options.
