# IEx Wasm

This project aims to be an Elixir IEx running fully in the browser via WASM via AtomVM.

To build, type:

```sh
mix deps.get
mix popcorn.cook
```

You can serve it by running

```sh
mix popcorn.server
```

and it'll be available at <http://localhost:4000>
