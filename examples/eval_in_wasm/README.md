# IEx Wasm

This project aims to be an Elixir IEx running fully in the browser via WASM via AtomVM.

For now, it's a textarea where you can type Erlang code and run it (fully in browser, via WASM via AtomVM).
It uses Popcorn under the hood.

To build, type:

```sh
mix deps.get
mix popcorn.build_runtime --target wasm --out-dir static # ensure git can access your private SSH key if sourcing from private repository
mix compile # or `MIX_ENV=prod mix compile` to disable tracing and build in release
```

This will generate the `static/eval_in_wasm.avm`, `static/AtomVM.js` and `static/AtomVM.wasm` files,
which along with other files in the `static` directory constitute a static website with the project.

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

## Deploy

- Build `.avm` file and AtomVM WASM binary in release mode (`MIX_ENV=prod mix compile`).
- Connect to server via ssh.
- Copy files into the server (`scp -A -r <SOURCE_DIR> root@<IP>:/var/www/wasm-$(date "+%Y-%m-%d")`).
- Update symlink to point to new directory (in `/var/www/`: `ln -sf ./wasm-DATE html`)
