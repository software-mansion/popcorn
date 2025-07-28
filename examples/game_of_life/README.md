# GameOfLife Popcorn demo

Process-based simulation of Conway's Game of Life (See <https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life>) running with Popcorn

## Code usage

With the `GameOfLife.Application` started, the simulation can be run by calling `GameOfLife.Supervisor.start_link` and then calling `GameOfLife.Grid.tick` to trigger next generation.

See the [AVM entrypoint](./lib/game_of_life.ex)

## Running on AtomVM

1. `mix deps.get`
2. `mix compile`
3. `/path_to_atom_vm _build/dev/bundle.avm`

## Running in WASM

1. `mix deps.get`
2. `mix compile`
3. Copy or symlink `AtomVM.js` & `AtomVM.wasm` into `wasm` folder
4. Run `elixir wasm/server.exs`
5. Open `http://localhost:4000/index.html`
6. Check the console for output
