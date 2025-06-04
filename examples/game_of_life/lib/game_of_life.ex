defmodule GameOfLife do
  @moduledoc """
  The entrypoint for AVM bundle. See `start/0`
  """

  alias Popcorn.Wasm

  @doc false
  def child_spec(_arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :run, []},
      restart: :temporary
    }
  end

  @doc """
  Main one-time script for GoL init
  """
  def run() do
    IO.puts("Running...\n")
    Wasm.register("noop")
    :ignore
  end
end
