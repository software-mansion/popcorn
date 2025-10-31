defmodule LocalLiveView.JS do
  @moduledoc """
  Module responsible for running JS in the browser.
  """

  @doc """
  Updates the LocalLiveView content by rendering new data inside the div with data-pop-view attribute.
  """
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
