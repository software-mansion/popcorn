defmodule LocalLiveView.JS do
  def rerender(rendered, view) do
    res = Popcorn.Wasm.run_js("""
    ({ args }) => {
      let elements = document.querySelectorAll('[data-pop-view]');
      elements = Array.from(elements)
      const found = elements.find((element) => element.getAttribute("data-pop-view") == args.view);
      if(found) {
        found.innerHTML = args.rendered;
      }
      document.dispatchEvent(new CustomEvent('popRender'));
    }
    """, %{rendered: rendered, view: view})
  end
end
