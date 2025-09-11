import { Popcorn } from "./wasm/popcorn.js";

var popRender = new CustomEvent('popRender', {});

function clickHandler(event) {
}

async function setup() {
  const popcorn = await Popcorn.init({
    debug: true
  })

  var afterRenderBind = async () => {
    const elements = document.querySelectorAll('[pop-click]');
    elements.forEach(el => {
      if (el.hasAttribute('pop-click')) {
        el.addEventListener('click', async (e) => {
          try {
            console.log(e)
            var view = find_view(el)
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
  document.addEventListener("popRender", () => afterRenderBind());
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
  console.log(elements)
  const found = elements.find((element) => element.getAttribute("data-pop-view") == view);
  return found
}

await setup();