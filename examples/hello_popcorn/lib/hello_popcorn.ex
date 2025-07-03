defmodule HelloPopcorn do
  def start() do
    Popcorn.Wasm.register("main")
    IO.puts("Hello console!")

    Popcorn.Wasm.run_js("""
    () => {
      document.body.innerHTML = "Hello from WASM!";
    }
    """)

    :ok
  end
end
