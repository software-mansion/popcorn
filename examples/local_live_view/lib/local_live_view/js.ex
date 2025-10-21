defmodule LocalLiveView.JS do
  def rerender(rendered, view, attribute \\ "data-pop-view") do
    view = view |> Module.split() |> List.last()

    res =
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
        %{rendered: rendered, view: view}
      )
  end
end
