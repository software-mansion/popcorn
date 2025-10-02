defmodule LocalLiveView.JS do
  def rerender(rendered, view, attribute \\ "data-pop-view") do
    view = view |> Module.split() |> List.last()

    res =
      Popcorn.Wasm.run_js(
        """
        ({ args }) => {
          let elements = document.querySelectorAll('[#{attribute}]');
          elements = Array.from(elements)
          const found = elements.find((element) => element.getAttribute("#{attribute}") == args.view);
          if(found) {
            found.innerHTML = args.rendered;
            document.dispatchEvent(new CustomEvent('popRender', {detail: {view: args.view}}));
          }
        }
        """,
        %{rendered: rendered, view: view}
      )
  end
end
