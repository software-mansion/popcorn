defmodule Mirror.PlayLocal do
  use LocalLiveView.Mirror

  @grid_cols 25

  @impl true
  def handle_sync(local_assigns, mirror_assigns) do
    my_cells = Map.get(local_assigns, "my_cells", %{})
    prev_cells = Map.get(mirror_assigns, "my_cells", %{})

    Enum.each(my_cells, fn {k_str, cell} ->
      unless Map.has_key?(prev_cells, k_str) do
        k = String.to_integer(k_str)

        ImmortalGrid.GridState.claim_cell(
          rem(k, @grid_cols),
          div(k, @grid_cols),
          cell["nick"],
          cell["owner_id"],
          cell["timestamp"] || 0,
          cell["claimed_offline"] || false
        )
      end
    end)

    Enum.each(prev_cells, fn {k_str, cell} ->
      unless Map.has_key?(my_cells, k_str) do
        k = String.to_integer(k_str)

        ImmortalGrid.GridState.release_cell(
          rem(k, @grid_cols),
          div(k, @grid_cols),
          cell["owner_id"]
        )
      end
    end)

    {:ok, local_assigns}
  end
end
