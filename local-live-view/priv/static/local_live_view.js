import { Popcorn } from "./popcorn.js";

export async function setup(liveSocket) {
  const popcorn = await Popcorn.init({
    debug: true,
    bundlePath: "../local_live_view/wasm/bundle.avm",
    wasmDir: "./local_live_view/wasm/"
  })
  
  const { data, durationMs } = await popcorn.call({ "views": find_predefined_views()}, {
    timeoutMs: 10_000,
  });
  
  const POP_VIEW = "data-pop-view"
  const PHX_ROOT_ID = "data-phx-root-id"
  const PHX_SESSION = "data-phx-session"
  const query = `[${POP_VIEW}]`
  const pop_view_els = Array.from(document.querySelectorAll(query));
  console.log({roots: {...liveSocket.roots}, pop_view_els: pop_view_els})
  
  pop_view_els.forEach((pop_view_el) => {
//  create a view
    const view = liveSocket.newRootView(pop_view_el);
//  update key functions within a view to call popcorn instead of pushing to the channel
    view.pushWithReply = async function(refGenerator, event, payload) {
      const { data, durationMs } = await popcorn.call({ "view": this.id, "event": event, 
        "payload": payload }, {
        timeoutMs: 10_000,
      });
      console.log({popcorn_call: { "view": this.id, "event": event, "payload": payload }})
      return new Promise((resolve, reject) => {})
    }
    view.join = function(_callback) {
      this.el.setAttribute(PHX_ROOT_ID, this.root.id);
      this.el.setAttribute(PHX_SESSION, this.root.id);
    }
    view.isConnected = function() { return true } //  todo implement it properly so that it returns weather we are connected to popcorn
//  run necessary callbacks
    view.join()
  });
  
  window.addEventListener("phx:llv_server_message", async (e) => {
    const { data, durationMs } = await popcorn.call({ "view": e.detail.view, "event": "llv_server_message", 
      "payload": { "event": "llv_server_message", "value": e.detail.payload, type: "llv_server_message" } }, {
      timeoutMs: 10_000,
    });
  });
  window.addEventListener("phx:llv_rerender", async (e) => {
    console.log("llv_rerender event received", e.detail.view)
    console.log("llv_rerender event roots view", liveSocket.roots[e.detail.view])
    liveSocket.roots[e.detail.view].join()
    const { data, durationMs } = await popcorn.call({ "view": e.detail.view, "event": "llv_rerender", 
      "payload": { "event": "llv_rerender", type: "llv_server_message" } }, {
      timeoutMs: 10_000,
    });
  });
}

function find_predefined_views() {
  let elements = document.querySelectorAll('[data-pop-view]');
  elements = Array.from(elements)
  return elements.map(el => el.getAttribute("data-pop-view"))
}
