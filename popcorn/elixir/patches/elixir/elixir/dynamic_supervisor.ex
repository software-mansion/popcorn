defmodule DynamicSupervisor do
  @compile {:popcorn_patch_private, add_restart: 1}
  def add_restart(state) do
    %{max_seconds: max_seconds, max_restarts: max_restarts, restarts: restarts} = state

    now = :erlang.monotonic_time(:second)
    restarts = :popcorn_module.add_restart([now | restarts], now, max_seconds)
    state = %{state | restarts: restarts}

    if length(restarts) <= max_restarts do
      {:ok, state}
    else
      {:shutdown, state}
    end
  end
end
