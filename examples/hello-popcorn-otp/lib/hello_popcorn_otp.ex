defmodule HelloPopcornOtp do
  use GenServer

  # `:wasm` only exists inside the OTP/WASM runtime, not on the host BEAM that
  # compiles this code, so silence the "module not available" warning.
  @compile {:no_warn_undefined, :wasm}

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    IO.puts("Hello console!")

    :wasm.run_js(
      """
      () => {
        document.body.innerHTML = "Hello from WASM!";
      }
      """,
      %{}
    )

    :ignore
  end
end
