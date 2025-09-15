defmodule LocalLiveView.JS do
  
  def render_predefined(rendered, view) do
    rerender(rendered, view, "data-pre-pop-view")
  end

  def rerender(rendered, view, attribute \\ "data-pop-view") do
    IO.puts "RENDER #{inspect({rendered, view, attribute})}"
    view = view |> Module.split() |> List.last()
    res = Popcorn.Wasm.run_js("""
    ({ args }) => {
      let elements = document.querySelectorAll('[#{attribute}]');
      elements = Array.from(elements)
      console.log(elements)
      console.log(args.view)
      console.log(args.rendered)
      const found = elements.find((element) => element.getAttribute("#{attribute}") == args.view);
      console.log(found)
      if(found) {
        found.innerHTML = args.rendered;
        document.dispatchEvent(new CustomEvent('popRender', {detail: {view: args.view}}));
      }
    }
    """, %{rendered: rendered, view: view})
  end
  
end
