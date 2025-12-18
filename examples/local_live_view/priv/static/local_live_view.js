import { Popcorn } from "./wasm/popcorn.js";

const events = [
    {js_event: "click", pop_event: "pop-click"},
    {js_event: "keydown", pop_event: "pop-keydown"},
    {js_event: "submit", pop_event: "pop-submit"},
    {js_event: "input", pop_event: "pop-change"}
]

let focused_el = document.activeElement;

async function setup() {
  const popcorn = await Popcorn.init({
    debug: true,
    bundlePath: "../local_live_view/wasm/bundle.avm",
    wasmDir: "./local_live_view/wasm/"
  })
  document.addEventListener("popRender", (event) => { afterRenderBind(event, popcorn) });
  const { data, durationMs } = await popcorn.call({ "views": find_predefined_views()}, {
    timeoutMs: 10_000,
  });
}

async function afterRenderBind(renderEvent, popcorn) {
  const view = renderEvent.detail.view
  const view_element = find_element(view)
  if (view_element.dataset.popcornBound) return;
  focused_el.focus()
  events.forEach(({js_event: js_event, pop_event: pop_event}) => {
    view_element.addEventListener(js_event, async (e) => {
      const target_el = e.target.closest(`[${pop_event}]`);
      console.log({event: e, target_el: target_el, pop_event: pop_event})
      if (target_el && view_element.contains(target_el)) {
        handlePopEvent(e, target_el, pop_event, popcorn, view);
      }
    });
  })
  window.addEventListener("phx:llv_server_message", async (e) => {
    const { data, durationMs } = await popcorn.call({ "view": e.detail.view, "event": "llv_server_message", 
      "payload": { "event": "llv_server_message", "value": e.detail.payload } }, {
      timeoutMs: 10_000,
    });
  });
  window.addEventListener("phx:llv_rerender", async (e) => {
    const { data, durationMs } = await popcorn.call({ "view": e.detail.view, "event": "llv_rerender", 
      "payload": { "event": "llv_rerender" } }, {
      timeoutMs: 10_000,
    });
  });
  view_element.dataset.popcornBound = "true";
}

async function handlePopEvent(e, target_el, pop_event, popcorn, view) {
  focused_el = document.activeElement
  const value = extractElementValue(e, target_el)
  try {
    const { data, durationMs } = await popcorn.call({ "view": view, "event": pop_event, 
      "payload": { "event": target_el.getAttribute(pop_event), "value": value } }, {
      timeoutMs: 10_000,
    });
  } catch (error) {
    console.log(error)
  }
}

function extractElementValue(e, el) {
  let value
  if ( el instanceof HTMLFormElement ) {
     e.preventDefault();
     const inputs = el.querySelectorAll('input');
     const inputsArray = Array.from(inputs)
     value = inputsArray.reduce((acc, input) => {
       acc[input.name] = input.value;
       return acc;
     }, {});
   } else if ( el.hasAttribute(value) )  {
     value = el.value;
   }
   return value;
};

// TODO this is a mock for heex templating inside index.html file
function find_predefined_views() {
  let elements = document.querySelectorAll('[data-pop-view]');
  elements = Array.from(elements)
  return elements.map(el => el.getAttribute("data-pop-view"))
}

function find_view(element) {
  while(element) {
    if(element.hasAttribute("data-pop-view")) {
        return element.getAttribute("data-pop-view")
    }
    element = element.parentElement
  }
  return null
}

function find_element(view) {
  let elements = document.querySelectorAll('[data-pop-view]');
  elements = Array.from(elements)
  const found = elements.find((element) => element.getAttribute("data-pop-view") == view);
  return found
}

await setup();