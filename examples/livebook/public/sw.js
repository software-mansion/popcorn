// Sends the Livebook iframe's requests to the VM in the shell page.
//
// The shell page (index.html) owns the VM, so it outlives the iframe's
// navigations. Static files and the shell's own requests go to the network.

const STATIC = ["/assets/", "/images/", "/favicons/", "/iframe/", "/out/", "/popcorn-ws.js", "/sw.js", "/robots.txt"];

// Responses built here must keep the iframe cross-origin isolated, like the shell
const ISOLATION = {
  "cross-origin-embedder-policy": "require-corp",
  "cross-origin-opener-policy": "same-origin",
  "cross-origin-resource-policy": "same-origin",
};

self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", (event) => event.waitUntil(self.clients.claim()));

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if (url.origin !== location.origin) return;
  if (STATIC.some((prefix) => url.pathname.startsWith(prefix))) return;

  if (event.request.mode === "navigate") {
    if (event.request.destination === "iframe") event.respondWith(toVm(event.request));
    return;
  }

  event.respondWith(
    self.clients.get(event.clientId).then((client) =>
      client?.frameType === "nested" ? toVm(event.request) : fetch(event.request),
    ),
  );
});

async function shell() {
  const windows = await self.clients.matchAll({ type: "window", includeUncontrolled: true });
  return windows.find((client) => client.frameType === "top-level");
}

async function toVm(request) {
  const target = await shell();
  if (!target) return new Response("Livebook's page isn't open", { status: 503 });

  const reqBody = ["GET", "HEAD"].includes(request.method) ? "" : new Uint8Array(await request.arrayBuffer());
  const channel = new MessageChannel();
  const reply = new Promise((resolve) => (channel.port1.onmessage = (event) => resolve(event.data)));

  target.postMessage(
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
  const body =
    request.mode === "navigate" && html && typeof respBody === "string"
      ? respBody.replace(/<head[^>]*>/i, (head) => `${head}<script src="/popcorn-ws.js"></script>`)
      : respBody;

  return new Response(nullBody ? null : body, {
    status,
    headers: [...headers.filter(([name]) => name !== "set-cookie"), ...Object.entries(ISOLATION)],
  });
}
