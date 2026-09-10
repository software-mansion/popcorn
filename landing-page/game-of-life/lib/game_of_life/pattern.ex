defmodule GameOfLife.Pattern do
  @patterns %{
    "rpentomino" => [[1, 0], [2, 0], [0, 1], [1, 1], [1, 2]],
    "glider" => [[1, 0], [2, 1], [0, 2], [1, 2], [2, 2]]
  }

  @pulsar for offset <- [2, 3, 4, 8, 9, 10],
              edge <- [0, 5, 7, 12],
              point <- [[edge, offset], [offset, edge]],
              do: point

  def centered("random", rows, cols) do
    for x <- 0..(rows - 1), y <- 0..(cols - 1), :rand.uniform() < 0.28, do: [x, y]
  end

  def centered("pulsar", rows, cols), do: center(@pulsar, rows, cols)
  def centered(name, rows, cols), do: center(Map.fetch!(@patterns, name), rows, cols)

  defp center(points, rows, cols) do
    max_x = points |> Enum.map(&hd/1) |> Enum.max()
    max_y = points |> Enum.map(&List.last/1) |> Enum.max()
    x_offset = div(rows - max_x, 2)
    y_offset = div(cols - max_y, 2)
    Enum.map(points, fn [x, y] -> [x + x_offset, y + y_offset] end)
  end
end
