import type { Socket as PhoenixSocket, SocketConnectOption } from "phoenix";
import type { TransportFrame } from "./types";
import type { PopcornClient } from "./index";

const LV_TOPIC_PREFIX = "lv:";
const llvIdFromTopic = (topic: string) =>
  topic.startsWith(LV_TOPIC_PREFIX) ? topic.slice(LV_TOPIC_PREFIX.length) : null;

// LiveView retries a join after this timeout, we don't want that
// thus we keep the timeout 'big enough'
const JOIN_TIMEOUT_MS = 120_000;

export interface PopcornLink {
  /** The never-networked Phoenix socket the LLV views' channels live on. */
  socket: PhoenixSocket;
  /** Deliver an inbound frame (out-of-band diffs) to the channel layer. */
  inject(frame: TransportFrame): void;
}

// A fake socket that connects LLV views to Popcorn.
// Phoenix channels can be normally constructed on top of this socket.
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

    send(frame: TransportFrame): void {
      // Ack heartbeats right away, as Wasm could
      // theoretically be late to ack and that would
      // kill all LLVs.
      if (frame.event == "heartbeat") {
        this.ack(frame, "ok", {});
        return;
      }

      const id = llvIdFromTopic(frame.topic);
      if (id === null) {
        this.ack(frame, "error", { reason: `unsupported channel ${frame.topic}` });
        return;
      }

      pop
        .call({ action: "transport_frame", id, frame }, { suppressErrorLog: true })
        .then((result) => {
          if (!result.ok) this.ack(frame, "error", result.error);
        });
    }

    // Ack an outbound frame in place (heartbeats, rejected frames).
    ack(frame: TransportFrame, status: string, response: unknown): void {
      this.inject({
        topic: frame.topic,
        event: "phx_reply",
        payload: { status, response },
        ref: frame.ref,
        join_ref: frame.join_ref,
      });
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
    timeout: JOIN_TIMEOUT_MS,
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
