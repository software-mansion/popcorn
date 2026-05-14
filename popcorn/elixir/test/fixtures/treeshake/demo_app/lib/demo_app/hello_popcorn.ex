defmodule HelloPopcorn do
  @moduledoc false
  use GenServer

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    IO.puts("Hello console!")

    :ignore
  end
end
