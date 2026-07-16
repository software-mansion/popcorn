import type { LLVSocket, PointerData } from "./types";

// Phoenix LiveView only binds click/keydown/keyup/blur/focus natively, so
// phx-mouse*/phx-window-mouse* and the HTML5 phx-drag* bindings are no-ops.
// Wire them up here, walking up from e.target to find the closest ancestor with
// the binding (same as how phx-click works) so element-level bindings on
// container elements also catch events from their descendants.
export function registerCustomEventBindings(socket: LLVSocket) {
  // The binding element's bounding rect is included so handlers can
  // position-aware-route the event (e.g. clientY - rect.top for the offset
  // within the element, rect.height for its size).
  const buildPointerData = (e: MouseEvent, el: Element): PointerData => {
    const rect = el.getBoundingClientRect();
    return {
      clientX: e.clientX,
      clientY: e.clientY,
      pageX: e.pageX,
      pageY: e.pageY,
      screenX: e.screenX,
      screenY: e.screenY,
      movementX: e.movementX,
      movementY: e.movementY,
      button: e.button,
      buttons: e.buttons,
      altKey: e.altKey,
      ctrlKey: e.ctrlKey,
      metaKey: e.metaKey,
      shiftKey: e.shiftKey,
      rect: {
        top: rect.top,
        left: rect.left,
        width: rect.width,
        height: rect.height,
      },
    };
  };

  const isElement = (node: Node | null): node is Element => node?.nodeType === Node.ELEMENT_NODE;

  // Walk up from `target` to the nearest element that carries the `binding`
  // attribute (e.g. "phx-dragover"), returning it or null if none is found.
  // Mirrors how phx-click resolves a handler from the event's deepest target.
  const closestWithBinding = (target: EventTarget | null, binding: string): Element | null => {
    let el = target as Node | null;
    while (el && isElement(el) && !el.getAttribute(binding)) {
      el = el.parentNode;
    }
    return isElement(el) && el.getAttribute(binding) ? el : null;
  };

  const mouseEventTypes = ["mousedown", "mouseup", "mousemove", "mouseover", "mouseout"] as const;

  for (const eventType of mouseEventTypes) {
    const elementBinding = `phx-${eventType}`;
    const windowBinding = `phx-window-${eventType}`;
    window.addEventListener(eventType, (e: MouseEvent) => {
      const el = closestWithBinding(e.target, elementBinding);
      if (el) {
        const phxEvent = el.getAttribute(elementBinding)!;
        socket.debounce(el, e, eventType, () => {
          socket.withinOwners(el, (view) => {
            view.pushEvent(eventType, el, null, phxEvent, buildPointerData(e, el), {});
          });
        });
      } else {
        document.querySelectorAll<Element>(`[${windowBinding}]`).forEach((wel) => {
          const phxEvent = wel.getAttribute(windowBinding)!;
          socket.debounce(wel, e, eventType, () => {
            socket.withinOwners(wel, (view) => {
              view.pushEvent(eventType, wel, null, phxEvent, buildPointerData(e, wel), {});
            });
          });
        });
      }
    });
  }

  // HTML5 drag-and-drop. The browser only allows a drop — and shows the move
  // cursor — when dragenter/dragover call preventDefault() on a drop target,
  // and drop must preventDefault() too. dragstart seeds dataTransfer so Firefox
  // actually initiates the drag. Bindings are read off phx-drag* attributes and
  // dispatched just like the mouse events above, with the same pointer data so
  // handlers can use the rect to decide drop position.
  const dragEventTypes = [
    "dragstart",
    "dragenter",
    "dragover",
    "dragleave",
    "drop",
    "dragend",
  ] as const;

  const dragPreventDefault = new Set<string>(["dragenter", "dragover", "drop"]);

  for (const eventType of dragEventTypes) {
    const elementBinding = `phx-${eventType}`;
    window.addEventListener(eventType, (e: DragEvent) => {
      const el = closestWithBinding(e.target, elementBinding);
      if (!el) return;

      // preventDefault must run on every event (not just the throttled ones)
      // or the browser keeps rejecting the drop mid-drag.
      if (dragPreventDefault.has(eventType)) {
        e.preventDefault();
      }
      if (e.dataTransfer) {
        if (eventType === "dragstart") {
          e.dataTransfer.effectAllowed = "move";
          try {
            e.dataTransfer.setData("text/plain", el.id || "");
          } catch {
            // setData throws in some browsers outside a trusted dragstart context
          }
        } else if (eventType === "dragover") {
          e.dataTransfer.dropEffect = "move";
        }
      }

      const phxEvent = el.getAttribute(elementBinding)!;
      socket.debounce(el, e, eventType, () => {
        socket.withinOwners(el, (view) => {
          view.pushEvent(eventType, el, null, phxEvent, buildPointerData(e, el), {});
        });
      });
    });
  }
}
