defmodule GameOfLife do
  @moduledoc """
  The entrypoint for AVM bundle. See `start/0`
  """

  alias GameOfLife.Grid
  alias FissionLib.Wasm

  @doc """
  A simple, example simulation with 3x3 grid and 5 generations
  """
  def start() do
    puts("Starting simulation\n")
    _ = GameOfLife.Application.start(:normal, [])
    Wasm.register("main")

    {:ok, _sup, %{grid_pid: pid}} =
      GameOfLife.Supervisor.start_simulation(3, 3, [{0, 1}, {1, 1}, {2, 1}])

    grid = Grid.status(pid)
    grid_str = print_grid(grid)
    puts(grid_str)
    puts("\n")

    for _ <- 1..5 do
      _ = Grid.tick(pid)
    end

    grid = Grid.tick(pid)
    grid_str = print_grid(grid)
    puts(grid_str)
    puts("\n")
    puts("Simulation ended\n")

    Process.sleep(:infinity)
  end

  defp puts(str) do
    # Console is missing in Elixir, IO.puts fails on AtomVM
    IO.puts(str)
  end

  defp print_grid(grid) do
    {:ok, str} = StringIO.open("")

    for row <- grid do
      for alive? <- row do
        if alive? do
          IO.write(str, " 1")
        else
          IO.write(str, " 0")
        end
      end

      IO.write(str, "\n")
    end

    {:ok, {_in, out}} = StringIO.close(str)
    out
  end
end
