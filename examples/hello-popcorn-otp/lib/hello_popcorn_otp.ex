defmodule HelloPopcornOtp do
  use GenServer

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    IO.puts("Hello console!")

    {:ok, _} =
      Popcorn.Wasm.run_js("""
      () => {
        document.body.innerHTML = "Hello from WASM!";
      }
      """)

    :ignore
  end
end
