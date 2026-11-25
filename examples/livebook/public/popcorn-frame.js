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
  window.popcornFrame = { failed, leaving: false };

  // Leaving the page disconnects its LiveView, which the shell doesn't show as
  // a lost connection. Going first, this listener runs before Phoenix's.
  window.addEventListener("pagehide", () => {
    window.popcornFrame.leaving = true;
  });

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

    // Livebook opens some of its own pages in a new tab, like the dashboard,
    // where a new shell would open its own Livebook on its home page (see
    // sw.js). They open here instead, in the tab's Livebook, whose paths are
    // under the tab's. So do clicks that ask for a new tab, with Cmd or Ctrl.
    document.addEventListener("click", (event) => {
      const link = event.target.closest?.("a[target='_blank'][href]");
      if (!link || event.defaultPrevented) return;
      const url = new URL(link.href, location.href);
      if (url.origin === location.origin && url.pathname.startsWith(`${bridge.base}/`)) {
        event.preventDefault();
        location.assign(url.href);
      }
    });
  }
}
