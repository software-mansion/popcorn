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
          let selection_range = {start: 0, end: 0};
          let focus_after_render = false
          const view_el = view_nodes[0];
          const active_el = document.activeElement;
          if(view_el.contains(active_el)) {
            focus_after_render = true;
            let el = view_el;
            let i = 0;
            if(active_el.nodeName === 'INPUT') {
              selection_range = { start: active_el.selectionStart, end: active_el.selectionEnd };
            }
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
              let type = el.getAttribute("type");
              el.setAttribute("type", "text");
              el.setSelectionRange(selection_range.start, selection_range.end);
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
