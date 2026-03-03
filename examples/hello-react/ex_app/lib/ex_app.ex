defmodule ExApp do
  use GenServer

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    Popcorn.Wasm.register(@process_name)
    IO.puts("Hello console!")

    # Popcorn.Wasm.run_js("""
    # () => {
    #   document.body.innerHTML = "Hello from WASM!";
    # }
    # """)

    :ignore
  end
end
