defmodule LocalLiveView.ServerSocket do
  @moduledoc false
  #  Module responsible for rendering a local live view in DOM.

  @doc false
  def send(event, payload, view, attribute \\ "data-pop-view") do
    view = view |> Module.split() |> List.last()

    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        const view_nodes = document.querySelectorAll(`[#{attribute}=${args.view}]`);
        if(view_nodes.length == 1) {
          let view_el = view_nodes[0];
          view_el.dispatchEvent(new CustomEvent("serverSend", { detail: { payload: args.payload, view: args.view, event_name: args.event_name } }));
        }
      }
      """,
      %{payload: payload, view: view, event_name: event}
    )
  end
end
