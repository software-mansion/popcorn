defmodule App.Application do
  def start do
    {:ok, _} = GenServer.start_link(App, [])
    IO.puts("Starting interpreter...")
    Process.sleep(:infinity)
  end
end
