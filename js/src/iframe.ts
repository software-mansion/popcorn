export function main(this: Window) {
  console.log("Hello from iframe", {
    w: this.window,
    isIframe: this.window.self !== this.window.top,
  });
}
