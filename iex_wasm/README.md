# IEx Wasm

This project aims to be an Elixir IEx running fully in the browser via WASM via AtomVM.

For now, it's a textarea where you can type Erlang code and run it (fully in browser, via WASM via AtomVM).

It uses FissionLib under the hood.

To build, type:

```sh
mix deps.get
mix fission_lib.build_avm --target wasm --out-dir static
mix compile
```

This will generate the `static/iex_wasm.avm`, `static/AtomVM.js` and `static/AtomVM.wasm` files, which along with other files in the `static` directory constitute a static website with the project.

You can serve it by running

```sh
elixir server.exs
```

and it'll be available at http://localhost:4000

The page can be served by any other HTTP server as well, but it must add the following response headers:
```
Cross-Origin-Opener-Policy: "same-origin"
Cross-Origin-Embedder-Policy: "require-corp"
```
for the WASM AtomVM to work.
