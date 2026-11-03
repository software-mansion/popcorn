// Loaded first in Livebook's pages: their WebSockets go to the VM through the
// shell page, as a Service Worker can't intercept WebSockets.
if (window.parent !== window && window.parent.popcornBridge) {
  window.WebSocket = window.parent.popcornBridge.WebSocket;
}
