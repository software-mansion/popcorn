defmodule GameOfLife do
  @moduledoc """
  The entrypoint for AVM bundle. See `start/0`
  """

  alias GameOfLife.Grid

  @doc """
  A simple, example simulation with 3x3 grid and 5 generations
  """
  def start() do
    {:ok, _app} = GameOfLife.Application.start(:normal, [])

    {:ok, _sup, %{grid_pid: pid}} =
      GameOfLife.Supervisor.start_simulation(3, 3, [{0, 1}, {1, 1}, {2, 1}])

    grid = Grid.status(pid)
    grid_str = print_grid(grid)
    puts(grid_str)
    puts("\n")

    for _ <- 1..5 do
      grid = Grid.tick(pid)
      grid_str = print_grid(grid)
      puts(grid_str)
      puts("\n")
    end

    :ok
  end

  defp puts(str) do
    # Console is missing in Elixir, IO.puts fails on AtomVM
    case Code.ensure_loaded(Console) do
      {:module, Console} ->
        Console.puts(str)

      _ ->
        IO.puts(str)
    end
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
