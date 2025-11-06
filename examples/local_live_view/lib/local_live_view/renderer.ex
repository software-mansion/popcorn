defmodule LocalLiveView.Renderer do
  @moduledoc false
  #  Module responsible for rendering a local live view in DOM.

  @doc false
  def rerender(content, view, attribute \\ "data-pop-view") do
    view = view |> Module.split() |> List.last()

    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        const view_nodes = document.querySelectorAll(`[#{attribute}=${args.view}]`);
        if(view_nodes.length == 1) {
          view_nodes[0].innerHTML = args.rendered;
          document.dispatchEvent(new CustomEvent('popRender', {detail: {view: args.view}}));
        }
      }
      """,
      %{rendered: content, view: view}
    )
  end
end
