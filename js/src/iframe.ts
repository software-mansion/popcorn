import init from "../assets/AtomVM.mjs";

export function main(this: Window) {
  init();
  console.log("Hello from iframe", {
    w: this.window,
    isIframe: this.window.self !== this.window.top,
  });
}
