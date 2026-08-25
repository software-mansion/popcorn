import type { VirtualTcpSocket } from "./virtual-tcp";

const ENCODER = new TextEncoder();
const DECODER = new TextDecoder();

export async function virtualFetch(
  input: RequestInfo | URL,
  init: RequestInit | undefined,
  connect: (host: string, port: number, options?: { signal?: AbortSignal }) => Promise<VirtualTcpSocket>,
  nativeFetch: typeof fetch,
  redirects = 0,
): Promise<Response> {
  const request = new Request(input, init);
  const url = new URL(request.url);
  if (!isVirtualHost(url.hostname)) return nativeFetch(request);
  if (url.protocol !== "http:") throw new TypeError("Virtual fetch supports http: URLs only");
  if (redirects > 20) throw new TypeError("redirect count exceeded");

  const socket = await connect(url.hostname, Number(url.port || 80), { signal: request.signal });
  const body = request.body === null ? new Uint8Array() : new Uint8Array(await request.arrayBuffer());
  const headers = new Headers(request.headers);
  headers.set("host", url.port === "" ? url.hostname : url.host);
  headers.set("connection", "close");
  if (body.byteLength > 0 && !headers.has("content-length")) headers.set("content-length", String(body.byteLength));

  const target = `${url.pathname || "/"}${url.search}`;
  let headerText = "";
  headers.forEach((value, name) => { headerText += `${name}: ${value}\r\n`; });
  const head = `${request.method} ${target} HTTP/1.1\r\n${headerText}\r\n`;
  await socket.write(ENCODER.encode(head));
  if (body.byteLength > 0) await socket.write(body);

  const reader = socket.readable.getReader();
  const parsed = await readHead(reader);
  const responseBody = hasResponseBody(request.method, parsed.status)
    ? responseStream(reader, parsed.rest, parsed.headers, socket)
    : null;
  const response = new Response(responseBody, { status: parsed.status, statusText: parsed.statusText, headers: parsed.headers });

  if (!isRedirect(parsed.status) || request.redirect === "manual") return response;
  if (request.redirect === "error") {
    socket.close();
    throw new TypeError("redirect mode is set to error");
  }
  const location = parsed.headers.get("location");
  if (location === null) return response;
  socket.close();
  const next = new URL(location, url);
  const method = parsed.status === 303 || ((parsed.status === 301 || parsed.status === 302) && request.method === "POST") ? "GET" : request.method;
  return virtualFetch(next, { method, headers: request.headers, redirect: request.redirect, signal: request.signal }, connect, nativeFetch, redirects + 1);
}

function isVirtualHost(host: string): boolean {
  return /^vm-\d+$/.test(host) || /^10\./.test(host);
}

async function readHead(reader: ReadableStreamDefaultReader<Uint8Array<ArrayBuffer>>): Promise<{ status: number; statusText: string; headers: Headers; rest: Uint8Array<ArrayBuffer> }> {
  let bytes = new Uint8Array(new ArrayBuffer(0));
  while (true) {
    const end = indexOf(bytes, ENCODER.encode("\r\n\r\n"));
    if (end !== -1) {
      const lines = DECODER.decode(bytes.slice(0, end)).split("\r\n");
      const match = /^HTTP\/1\.[01] (\d{3})(?: (.*))?$/.exec(lines.shift() ?? "");
      if (match === null) throw new TypeError("invalid HTTP response status line");
      const headers = new Headers();
      for (const line of lines) {
        const colon = line.indexOf(":");
        if (colon === -1) throw new TypeError("invalid HTTP response header");
        headers.append(line.slice(0, colon).trim(), line.slice(colon + 1).trim());
      }
      return { status: Number(match[1]), statusText: match[2] ?? "", headers, rest: bytes.slice(end + 4) };
    }
    const chunk = await reader.read();
    if (chunk.done) throw new TypeError("connection closed before HTTP response headers");
    bytes = concat(bytes, chunk.value);
  }
}

function responseStream(reader: ReadableStreamDefaultReader<Uint8Array<ArrayBuffer>>, first: Uint8Array<ArrayBuffer>, headers: Headers, socket: VirtualTcpSocket): ReadableStream<Uint8Array<ArrayBuffer>> {
  const lengthText = headers.get("content-length");
  let remaining = lengthText === null ? null : Number(lengthText);
  let buffered = first;
  return new ReadableStream({
    pull: async (controller) => {
      if (remaining === 0) {
        controller.close();
        socket.close();
        return;
      }
      const chunk = buffered.byteLength > 0 ? buffered : (await reader.read()).value;
      buffered = new Uint8Array(new ArrayBuffer(0));
      if (chunk === undefined) {
        if (remaining !== null) controller.error(new TypeError("HTTP response body ended early"));
        else controller.close();
        socket.close();
        return;
      }
      const output = remaining === null ? chunk : chunk.slice(0, remaining);
      if (remaining !== null) remaining -= output.byteLength;
      controller.enqueue(output);
    },
    cancel: () => socket.close(),
  });
}

function hasResponseBody(method: string, status: number): boolean { return method !== "HEAD" && status !== 204 && status !== 304 && (status < 100 || status >= 200); }
function isRedirect(status: number): boolean { return status === 301 || status === 302 || status === 303 || status === 307 || status === 308; }
function concat(left: Uint8Array<ArrayBuffer>, right: Uint8Array<ArrayBuffer>): Uint8Array<ArrayBuffer> {
  const result = new Uint8Array(new ArrayBuffer(left.byteLength + right.byteLength));
  result.set(left);
  result.set(right, left.byteLength);
  return result;
}
function indexOf(haystack: Uint8Array<ArrayBuffer>, needle: Uint8Array<ArrayBuffer>): number {
  outer: for (let index = 0; index <= haystack.byteLength - needle.byteLength; index += 1) {
    for (let offset = 0; offset < needle.byteLength; offset += 1) if (haystack[index + offset] !== needle[offset]) continue outer;
    return index;
  }
  return -1;
}
