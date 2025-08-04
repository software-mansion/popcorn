defmodule ElixirTour do
  use GenServer

  @process_name :elixir_tour

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    Popcorn.Wasm.register(@process_name)
    IO.puts("Hello console!")

    :ok
  end
end
