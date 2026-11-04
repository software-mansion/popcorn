// Loaded first in the tab's relay frame and in Livebook's pages, see sw.js.
{
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
    // created in the shell, with a socket process in the VM at their other end
    window.WebSocket = bridge.WebSocket;
  }
}
