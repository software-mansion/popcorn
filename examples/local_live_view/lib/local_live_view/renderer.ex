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
          let key = [];
          let focus_after_render = false
          const view_el = view_nodes[0];
          const active_el = document.activeElement;
          if(view_el.contains(active_el)) {
            focus_after_render = true;
            let el = view_el;
            let i = 0;
            while(el != active_el) {
              if(el.children[i].contains(active_el)) {
                key.push(i);
                el = el.children[i];
                i = 0;
              } else {
                i++;
              }
            }
          }
          view_el.innerHTML = args.rendered;
          document.dispatchEvent(new CustomEvent('popRender', {detail: {view: args.view}}));
          if(focus_after_render) {
            let el = view_el
            while(key.length != 0) {
              el = el.children[key.shift()]
            }
            if(el.nodeName === 'INPUT') {
              let length = el.value.length;
              let type = el.getAttribute("type");
              el.setAttribute("type", "text");
              el.setSelectionRange(length, length);
              el.setAttribute("type", type);
            }
            el.focus();
          }
        }
      }
      """,
      %{rendered: content, view: view}
    )
  end
end
