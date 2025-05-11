defmodule GameOfLife do
  @moduledoc """
  The entrypoint for AVM bundle. See `start/0`
  """

  alias FissionLib.Wasm

  @doc """
  A simple, example simulation with 3x3 grid and 5 generations
  """
  def start() do
    IO.puts("Init...\n")
    {:ok, _pid, _config} = :elixir.start([], [])
    _ = GameOfLife.Application.start(:normal, [])
    Wasm.register("noop")
    Process.sleep(:infinity)
  end
end
