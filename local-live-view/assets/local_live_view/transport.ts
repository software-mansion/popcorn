import type { Socket as PhoenixSocket, SocketConnectOption } from "phoenix";
import type { TransportFrame } from "./types";
import type { PopcornClient } from "./index";

// ---- Popcorn channel transport -----------------------------------------
//
// Each fake view gets a REAL Phoenix Channel on a dedicated Socket whose
// transport is the class below — the same public seam Phoenix.LongPoll
// plugs into. No network connection ever exists: the "wire" is
// popcorn.call one way and injected frames the other, and the socket's
// encode/decode passthrough keeps frames plain objects (no serializer).
//
// The transport honors the channel ack contract: frames the WASM view
// must answer (join, events, component-destruction bookkeeping) ride
// popcorn.call, whose promise the view's process settles once the frame
// is processed — the resolved {status, payload} (initial rendered, render
// diff or %{} no-op, pruned cids) becomes the channel ack. So LiveView's
// stock push lifecycle runs unmodified: apply the ack diff, undo the
// push's element refs, resolve the {:reply, ...} payload. Ref locks and
// phx-disable-with span the local round-trip exactly like a server
// round-trip.

const llvIdFromTopic = (topic: string) => topic.slice("lv:".length);

// Frames the WASM view must answer: the channel join (ack carries the
// initial rendered), DOM events (ack carries the render diff and reply)
// and component-destruction bookkeeping (ack carries the pruned cids).
// Anything else — heartbeats, phx_leave — is acked in place so the socket
// stays healthy and teardown completes. Each entry needs a matching
// Message clause in LocalLiveView.Server.
const ANSWERED_EVENTS = ["phx_join", "event", "cids_will_destroy", "cids_destroyed"];

export interface PopcornLink {
  /** The never-networked Phoenix socket the fake views' channels live on. */
  socket: PhoenixSocket;
  /** Deliver an inbound frame (out-of-band diffs) to the channel layer. */
  inject(frame: TransportFrame): void;
}

export function createPopcornSocket(
  SocketClass: typeof PhoenixSocket,
  pop: PopcornClient,
): PopcornLink {
  let transport: PopcornTransport | null = null;

  class PopcornTransport {
    readyState = 0; // CONNECTING
    onopen: () => void = () => {};
    onerror: (error: unknown) => void = () => {};
    onmessage: (event: { data: TransportFrame }) => void = () => {};
    onclose: (event: { code: number; wasClean: boolean }) => void = () => {};

    // The WebSocket-shaped signature the Socket constructs us with; the URL
    // is a dead label.
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    constructor(_endpointURL: string, _protocols?: unknown) {
      // eslint-disable-next-line @typescript-eslint/no-this-alias
      transport = this;
      // The Socket assigns onopen/onmessage/onclose after `new`, so the
      // "connection" must open asynchronously.
      queueMicrotask(() => {
        this.readyState = 1; // OPEN
        this.onopen();
      });
    }

    // Deliver an inbound frame. Async so an ack never re-enters Socket code
    // in the middle of an outbound send.
    inject(frame: TransportFrame): void {
      queueMicrotask(() => {
        if (this.readyState === 1) this.onmessage({ data: frame });
      });
    }

    // Ack an outbound frame in place (joins, leaves, heartbeats, no-ops).
    ack(frame: TransportFrame, status: string, response: unknown): void {
      this.inject({
        topic: frame.topic,
        event: "phx_reply",
        payload: { status, response },
        ref: frame.ref,
        join_ref: frame.join_ref,
      });
    }

    send(frame: TransportFrame): void {
      const { topic, event, payload } = frame;

      if (!ANSWERED_EVENTS.includes(event)) {
        this.ack(frame, "ok", {});
        return;
      }

      // The view's process settles the call promise with {status, payload}
      // once the frame is processed; a dispatcher-side reject resolves with
      // ok: false. A call left unsettled (crashed view process) rejects at
      // the call timeout. Every failure acks "error" — never "timeout",
      // whose Push path triggers liveSocket.reloadWithJitter.
      pop.transportFrame(llvIdFromTopic(topic), event, payload).then(
        (result) => {
          if (result.ok) {
            const { status, payload: response } = result.data as {
              status: string;
              payload: unknown;
            };
            this.ack(frame, status, response);
          } else {
            this.ack(frame, "error", result.error);
          }
        },
        (err) => this.ack(frame, "error", String(err)),
      );
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    close(_code?: number, _reason?: string): void {
      this.readyState = 3; // CLOSED
      queueMicrotask(() => this.onclose({ code: 1000, wasClean: true }));
    }
  }

  // The endpoint URL is only a label — the transport never dereferences it.
  const socket = new SocketClass("/llv-popcorn", {
    transport: PopcornTransport,
    encode: (payload: unknown, callback: (encoded: unknown) => void) => callback(payload),
    decode: (rawPayload: unknown, callback: (decoded: unknown) => void) => callback(rawPayload),
  } as unknown as Partial<SocketConnectOption>);
  socket.connect();

  return {
    socket,
    inject(frame: TransportFrame): void {
      transport?.inject(frame);
    },
  };
}
