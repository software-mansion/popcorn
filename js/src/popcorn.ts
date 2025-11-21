import { main } from "./iframe.js";

declare global {
  interface Window {
    runInIframeContext: (f: () => void) => void;
  }
}

export function init(container = document.body, id = "popcorn"): void {
  const { initIframe } = mountIframe(id, container);
  initIframe(main);
}

export function deinit(id = "popcorn"): void {
  const iframe = document.getElementById(id);
  if (iframe === null) {
    throw new Error(`Popcorn iframe with id '${id}' doesn't exist`);
  }
  iframe.remove();
}

function mountIframe(id: string, container: HTMLElement) {
  if (document.getElementById(id) !== null) {
    throw new Error(`Popcorn instance with id '${id}' is already mounted`);
  }

  const iframe = document.createElement("iframe");
  iframe.id = id;
  iframe.style = "visibility: hidden; width: 0px; height: 0px; border: none";
  iframe.srcdoc = "<html></html>";

  container.appendChild(iframe);

  if (iframe.contentWindow === null) {
    throw new Error("Invalid iframe origin");
  }
  iframe.contentWindow.runInIframeContext = function (f) {
    f.apply(iframe.contentWindow);
  };
  const initIframe = (f: () => void) => {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    iframe.contentWindow!.runInIframeContext(f);
  };
  return { iframe, initIframe };
}
