// Loaded first in the tab's relay frame and in Livebook's pages, see sw.js.
{
  // The page's scripts and stylesheets that fail to load, as when the server
  // has stopped, for the shell to show. Going first, this script sees them.
  const failed = [];
  window.addEventListener(
    "error",
    ({ target }) => {
      if (
        target.tagName === "SCRIPT" ||
        (target.tagName === "LINK" && target.rel === "stylesheet")
      )
        failed.push(target.src || target.href);
    },
    true,
  );
  window.popcornFrame = { failed };

  // Of the shell page, which runs the tab's VMs
  const bridge = window.parent === window ? null : window.parent.popcornBridge;

  if (bridge) {
    // The Service Worker sends the tab's requests to the relay frame
    navigator.serviceWorker.addEventListener("message", async (event) => {
      const req = event.data?.popcornRequest;
      if (req) event.ports[0].postMessage(await bridge.serve(req));
    });
    navigator.serviceWorker.startMessages();

    // Service Workers can't intercept WebSockets, so Livebook's pages' ones are
    // created in the shell, with a socket process in the VM at their other end.
    // Phoenix tells binary frames by this window's ArrayBuffer, which the
    // shell's would fail, so they're made here.
    window.WebSocket = class WebSocket extends bridge.WebSocket {
      binaryFrame(bytes) {
        const buffer = new ArrayBuffer(bytes.byteLength);
        new Uint8Array(buffer).set(bytes);
        return buffer;
      }
    };
  }
}
