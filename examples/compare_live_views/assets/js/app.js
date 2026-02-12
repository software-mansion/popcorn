// If you want to use Phoenix channels, run `mix help phx.gen.channel`
// to get started and then uncomment the line below.
// import "./user_socket.js"

// You can include dependencies in two ways.
//
// The simplest option is to put them in assets/vendor and
// import them using relative paths:
//
//     import "../vendor/some-package.js"
//
// Alternatively, you can `npm install some-package --prefix assets` and import
// them using a path starting with the package name:
//
//     import "some-package"
//
// If you have dependencies that try to import CSS, esbuild will generate a separate `app.css` file.
// To load it, simply add a second `<link>` to your `root.html.heex` file.

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html"
// Establish Phoenix Socket and LiveView configuration.
import {Socket} from "phoenix"
import {LiveSocket} from "phoenix_live_view"
import {hooks as colocatedHooks} from "phoenix-colocated/compare_live_views"
import topbar from "../vendor/topbar"

const csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content")
const liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: {_csrf_token: csrfToken},
  hooks: {...colocatedHooks},
})
console.log({roots: {...liveSocket.roots}})

// Show progress bar on live navigation and form submits
topbar.config({barColors: {0: "#29d"}, shadowColor: "rgba(0, 0, 0, .3)"})
window.addEventListener("phx:page-loading-start", _info => topbar.show(300))
window.addEventListener("phx:page-loading-stop", _info => topbar.hide())












// LOCAL LIVE VIEW START========================================================================================
import { Popcorn } from "../vendor/local_live_view/wasm/popcorn.js";

async function setup() {
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
      console.log({event: event, payload: payload})
      const { data, durationMs } = await popcorn.call({ "view": this.id, "event": event, 
        "payload": payload }, {
        timeoutMs: 10_000,
      });
      return new Promise((resolve, reject) => {})
    }
    view.join = function(_callback) {
      this.el.setAttribute(PHX_ROOT_ID, this.root.id);
      this.el.setAttribute(PHX_SESSION, this.root.id);
      console.log("joined!")
    }
    view.isConnected = function() { return true } //  todo implement it properly so that it returns weather we are connected to popcorn
//  run necessary callbacks
    view.join()
  });
}

function find_predefined_views() {
  let elements = document.querySelectorAll('[data-pop-view]');
  elements = Array.from(elements)
  return elements.map(el => el.getAttribute("data-pop-view"))
}

setup();
// LOCAL LIVE VIEW END========================================================================================

// connect if there are any LiveViews on the page
liveSocket.connect()
liveSocket.enableLatencySim(500)
// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket

// The lines below enable quality of life phoenix_live_reload
// development features:
//
//     1. stream server logs to the browser console
//     2. click on elements to jump to their definitions in your code editor
//
if (process.env.NODE_ENV === "development") {
  window.addEventListener("phx:live_reload:attached", ({detail: reloader}) => {
    // Enable server log streaming to client.
    // Disable with reloader.disableServerLogs()
    reloader.enableServerLogs()

    // Open configured PLUG_EDITOR at file:line of the clicked element's HEEx component
    //
    //   * click with "c" key pressed to open at caller location
    //   * click with "d" key pressed to open at function component definition location
    let keyDown
    window.addEventListener("keydown", e => keyDown = e.key)
    window.addEventListener("keyup", e => keyDown = null)
    window.addEventListener("click", e => {
      if(keyDown === "c"){
        e.preventDefault()
        e.stopImmediatePropagation()
        reloader.openEditorAtCaller(e.target)
      } else if(keyDown === "d"){
        e.preventDefault()
        e.stopImmediatePropagation()
        reloader.openEditorAtDef(e.target)
      }
    }, true)

    window.liveReloader = reloader
  })
}
