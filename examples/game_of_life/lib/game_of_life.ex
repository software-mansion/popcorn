defmodule GameOfLife do
  @moduledoc """
  A module intended to be the last child of  `GameOfLife.Application`,
  executing `run/0` when the app is ready.
  """

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
    :ignore
  end
end
