// Sends a tab's Livebook requests to the tab's VM, in the shell page.
//
// Each tab registers this worker for a path of its own, /tab/<id>/, where its
// Livebook runs (see index.html): a worker shared by tabs couldn't tell which
// tab's VM a navigation is for. So the worker's clients are the tab's frames:
// a relay frame, which passes the requests to the shell, Livebook's pages and
// their outputs' iframes.
//
// It doesn't call skipWaiting() or clients.claim(): a tab keeps the worker it
// registered, as an updated one might not fit its page.

const SCOPE = new URL(self.registration.scope).pathname;
const BASE = SCOPE.slice(0, -1);
const RELAY = `${SCOPE}popcorn-relay`;

// Static files load from the network, without the tab's path
const STATIC = ["/assets/", "/images/", "/favicons/", "/iframe/", "/out/", "/notebook_vm/", "/popcorn-frame.js", "/robots.txt"];

// Responses built here must keep the iframe cross-origin isolated, like the shell
const ISOLATION = {
  "cross-origin-embedder-policy": "require-corp",
  "cross-origin-opener-policy": "same-origin",
  "cross-origin-resource-policy": "same-origin",
};

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if (url.origin !== location.origin) return;
  if (url.pathname === RELAY) return event.respondWith(relayPage());

  const path = url.pathname.startsWith(SCOPE) ? url.pathname.slice(BASE.length) : url.pathname;
  if (STATIC.some((prefix) => path.startsWith(prefix))) {
    if (path !== url.pathname) event.respondWith(fromNetwork(new URL(path + url.search, location.origin)));
    return;
  }

  // The rest is Livebook's, also outside the tab's path
  event.respondWith(toVm(event.request));
});

function relayPage() {
  const html = `<!doctype html><script src="/popcorn-frame.js"></script>`;
  return new Response(html, { headers: { "content-type": "text/html; charset=utf-8", ...ISOLATION } });
}

// A response of its own, so that pages keep their URL under the tab's path.
// Its body is decoded already.
async function fromNetwork(url) {
  const resp = await fetch(url);
  const headers = new Headers(resp.headers);
  headers.delete("content-encoding");
  headers.delete("content-length");
  return new Response(resp.body, { status: resp.status, statusText: resp.statusText, headers });
}

async function findRelay() {
  const clients = await self.clients.matchAll({ type: "window" });
  return clients.find((client) => new URL(client.url).pathname === RELAY);
}

// The script goes first, so that it replaces the page's WebSocket
function addFrameScript(html) {
  return html.replace(/<head[^>]*>/i, (head) => `${head}<script src="/popcorn-frame.js"></script>`);
}

async function toVm(request) {
  const relay = await findRelay();
  if (!relay) return new Response("Livebook's tab isn't open", { status: 503 });

  const reqBody = ["GET", "HEAD"].includes(request.method) ? "" : new Uint8Array(await request.arrayBuffer());
  const channel = new MessageChannel();
  const reply = new Promise((resolve) => (channel.port1.onmessage = (event) => resolve(event.data)));

  relay.postMessage(
    { popcornRequest: { method: request.method, url: request.url, headers: [...request.headers], body: reqBody } },
    [channel.port2],
  );

  const { status, headers, body: respBody } = await reply;
  const location = headers.find(([name]) => name === "location")?.[1];

  if (status >= 300 && status < 400 && location) {
    return Response.redirect(new URL(location, request.url), status);
  }

  const nullBody = [101, 204, 205, 304].includes(status) || request.method === "HEAD";
  const html = headers.some(([name, value]) => name === "content-type" && value.startsWith("text/html"));
  const body = request.mode === "navigate" && html && typeof respBody === "string" ? addFrameScript(respBody) : respBody;

  return new Response(nullBody ? null : body, {
    status,
    headers: [...headers.filter(([name]) => name !== "set-cookie"), ...Object.entries(ISOLATION)],
  });
}
