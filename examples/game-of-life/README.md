# GameOfLife Popcorn demo

Process-based simulation of Conway's Game of Life (See <https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life>) running with Popcorn.

## Code usage

With the `GameOfLife.Application` started, the simulation can be run by calling `GameOfLife.Supervisor.start_link` and then calling `GameOfLife.Grid.tick` to trigger next generation.

See the [AVM entrypoint](./lib/game_of_life.ex)

## Usage

From the repository root:

```bash
pnpm install
mise run dev --example game-of-life
```

or directly from the example directory:

```bash
mix dev
```

and visit [localhost:4000](http://localhost:4000)
