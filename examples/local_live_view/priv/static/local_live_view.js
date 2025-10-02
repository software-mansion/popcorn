import { Popcorn } from "../wasm/popcorn.js";

async function setup() {
  const popcorn = await Popcorn.init({
    debug: true
  })

  var afterRenderBind = async (event) => {
    var view = event.detail.view
    var view_element = find_element(event.detail.view)
    const elements = view_element.querySelectorAll('[pop-click]');
    elements.forEach(el => {
      if (el.hasAttribute('pop-click')) {
        el.addEventListener('click', async (e) => {
          try {
            const { data, durationMs } = await popcorn.call({ "view": view, "event": "click", 
              payload: {"event": el.getAttribute('pop-click')} }, {
              timeoutMs: 10_000,
            });
          } catch (error) {
            console.log(error)
          }
        });
      }
    });
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