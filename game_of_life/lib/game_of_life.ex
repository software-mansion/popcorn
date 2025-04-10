defmodule GameOfLife do
  @moduledoc """
  Documentation for `GameOfLife`.
  """

  alias GameOfLife.{Cell, Grid, Supervisor}

  def start() do
    x = "Hello, FissionLib\n"
    puts(x)

    {:ok, _reg} = Registry.start_link(keys: :unique, name: GameOfLife.CellRegistry)
    # {:ok, pid} = Grid.start_link(3, 3, [{1, 1}])
    # grid = Grid.tick(pid)
    # grid_str = print_grid(grid)
    # puts(grid_str)
    # puts("\n")
  end

  defp puts(str) do
    # Console.puts(str)
    IO.puts(str)
  end

  defp print_grid(grid) do
    {:ok, str} = StringIO.open("")

    for row <- grid do
      for alive? <- row do
        if alive? do
          IO.write(str, " 0")
        else
          IO.write(str, " 1")
        end
      end

      IO.write(str, "\n")
    end

    {:ok, {_in, out}} = StringIO.close(str)
    out
  end
end
