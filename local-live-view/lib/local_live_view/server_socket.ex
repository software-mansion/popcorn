defmodule LocalLiveView.ServerSocket do
  @moduledoc false

  @doc false
  def send(event, payload, view) do
    view_name = view |> Module.split() |> List.last()

    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        if (window.__llvSync) {
          window.__llvSync(args.view, args.event_name, args.payload);
        }
      }
      """,
      %{payload: payload, view: view_name, event_name: event}
    )
  end
end
