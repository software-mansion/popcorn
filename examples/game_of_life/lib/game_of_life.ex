defmodule GameOfLife do
  @moduledoc """
  The entrypoint for AVM bundle. See `start/0`
  """

  alias Popcorn.Wasm

  @doc """
  Starts `GameOfLife.Application`
  """
  def start() do
    IO.puts("Init...\n")
    {:ok, _pid, _config} = :elixir.start([], [])
    _ = GameOfLife.Application.start(:normal, [])
    Wasm.register("noop")
    Process.sleep(:infinity)
  end
end
