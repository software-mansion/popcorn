defmodule LocalLiveView.ServerSocket do
  @moduledoc false
  #  Module responsible for rendering a local live view in DOM.

  @doc false
  def send(payload, view) do
    view = view |> Module.split() |> List.last()

    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        const liveSocket = window.liveSocket;
        if (liveSocket) {
          console.log(liveSocket)
          liveSocket.pushEvent("llv_message", {view: args.view, payload: args.payload})
        }
      }
      """,
      %{payload: payload, view: view}
    )
  end
end
