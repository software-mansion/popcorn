defmodule LocalLiveView do
  use GenServer
  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    Popcorn.Wasm.register(@process_name)
    rendered = 
      MyLocalComponent.greet(%{name: "Franek"})
      |> Phoenix.HTML.Safe.to_iodata()
      |> iodata_to_binary()
    Popcorn.Wasm.run_js("""
    ({ args }) => {
      document.body.innerHTML = args.rendered;
    }
    """, %{rendered: rendered})

    :ignore
  end

  def iodata_to_binary(list_of_binaries) when is_list(list_of_binaries) do
    Enum.reduce(list_of_binaries, "", fn binary, acc ->
      acc <> to_string(binary) 
    end)
  end
  
  def render(assigns) do
    :ok
  end
end
