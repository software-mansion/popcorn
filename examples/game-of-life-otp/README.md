# GameOfLife OTP demo

Process-based simulation of Conway's Game of Life (see
<https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life>) running with Popcorn
on the OTP/BEAM WebAssembly runtime. It is a port of the
[game-of-life](../game-of-life) AtomVM example.

Each cell is a `GenServer`, one simulation is a `Supervisor` tree, and the UI
(`lib/game_of_life/ui.ex`) drives the DOM through the preloaded `:wasm` module.

Install the JavaScript dependencies and build the OTP package from the
repository root:

```bash
pnpm install
mise run build-otp-js
```

Then start the example from the repository root:

```bash
mise run dev --example game-of-life-otp
```

or directly from the example directory:

```bash
mix dev
```

and visit [localhost:5173](http://localhost:5173).

This example also contains [Playwright](https://playwright.dev)-based tests. To
run them from the `assets` directory, type:

```bash
pnpm exec playwright install chromium
pnpm test
```
