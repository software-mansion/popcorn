import { Popcorn } from "./wasm/popcorn.js";

var events = [
    {js_event: "click", pop_event: "pop-click"},
    {js_event: "keydown", pop_event: "pop-keydown"},
    {js_event: "submit", pop_event: "pop-submit"},
    {js_event: "input", pop_event: "pop-change"}
]

var focused_el = document.activeElement;

async function setup() {
  const popcorn = await Popcorn.init({
    debug: true,
    bundlePath: "../local_live_view/wasm/bundle.avm",
    wasmDir: "./local_live_view/wasm/"
  })
  
  var afterRenderBind = async (renderEvent) => {
    var view = renderEvent.detail.view
    var view_element = find_element(renderEvent.detail.view)
    focused_el.focus()
    events.forEach(({js_event: event, pop_event: pop_event}) => {
      const elements = view_element.querySelectorAll("[" + pop_event + "]");
      elements.forEach(el => {
        if (el.hasAttribute(pop_event)) {
          el.addEventListener(event, async (e) => {
            focused_el = document.activeElement;
            var value
            if(el instanceof HTMLFormElement){
              e.preventDefault();
              const inputs = el.querySelectorAll('input');
              const inputsArray = Array.from(inputs)
              value = inputsArray.reduce((acc, input) => {
                acc[input.name] = input.value;
                return acc;
              }, {});
            } else if (el.hasAttribute(value))  {
              value = el.value;
            }
            try {
              const { data, durationMs } = await popcorn.call({ "view": view, "event": pop_event, 
                "payload": {"event": el.getAttribute(pop_event), "value": value} }, {
                timeoutMs: 10_000,
              });
            } catch (error) {
              console.log(error)
            }
          });
        }
      });
    })
  }
  document.addEventListener("popRender", afterRenderBind);
  const { data, durationMs } = await popcorn.call({ "views": find_predefined_views()}, {
    timeoutMs: 10_000,
  });
}

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