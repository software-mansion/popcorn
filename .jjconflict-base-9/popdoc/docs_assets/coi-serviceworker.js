/* coi-serviceworker - Cross-Origin Isolation via Service Worker
 * Injects COOP/COEP headers so SharedArrayBuffer works on localhost or hosts
 * where you can't control response headers directly.
 * Reference: https://github.com/gzuidhof/coi-serviceworker
 */

if (typeof window === "undefined") {
  self.addEventListener("install", () => self.skipWaiting());
  self.addEventListener("activate", (event) =>
    event.waitUntil(self.clients.claim())
  );

  self.addEventListener("fetch", (event) => {
    const request = event.request;
    if (request.cache === "only-if-cached" && request.mode !== "same-origin") {
      return;
    }

    event.respondWith(
      fetch(request).then((response) => {
        if (response.status === 0) return response;

        const headers = new Headers(response.headers);
        headers.set("Cross-Origin-Opener-Policy", "same-origin");
        headers.set("Cross-Origin-Embedder-Policy", "require-corp");
        headers.set("Cross-Origin-Resource-Policy", "cross-origin");

        return new Response(response.body, {
          status: response.status,
          statusText: response.statusText,
          headers
        });
      })
    );
  });
} else {
  (() => {
    if (crossOriginIsolated) return;

    navigator.serviceWorker
      .register(document.currentScript.src)
      .then((registration) => {
        if (registration.active) {
          window.sessionStorage.setItem("coiReload", "1");
          window.location.reload();
          return;
        }

        const sw = registration.installing || registration.waiting;

        sw?.addEventListener("statechange", (event) => {
          if (event.target.state === "activated") {
            window.sessionStorage.setItem("coiReload", "1");
            window.location.reload();
          }
        });
      })
      .catch((error) => {
        console.error("coi-serviceworker registration failed:", error);
      });
  })();
}
