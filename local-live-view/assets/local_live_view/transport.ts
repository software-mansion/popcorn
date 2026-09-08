import { Socket } from "phoenix";
import { LiveSocket, type Hook } from "phoenix_live_view";
import type { LLVSocket, TransportFrame } from "./types";
import type { PopcornClient } from "./index";

const LV_TOPIC_PREFIX = "lv:";
const llvIdFromTopic = (topic: string) =>
  topic.startsWith(LV_TOPIC_PREFIX) ? topic.slice(LV_TOPIC_PREFIX.length) : null;

// LiveView retries a join after this timeout, we don't want that
// thus we keep the timeout 'big enough'
const JOIN_TIMEOUT_MS = 120_000;

interface FrameSink {
  inject(frame: TransportFrame): void;
}

export interface PopcornTransports {
  /** Deliver an inbound frame (diff, reply) to the transport serving its topic. */
  route(frame: TransportFrame): void;
  /**
   * A LiveSocket dedicated to one view, riding its own fake transport: same
   * LiveSocket/Socket classes the host runs on, to be embedded in the view's
   * slot (which also opts it out of dead-view, main and history duties).
   */
  newSocket(llvId: string): LLVSocket;
}

// The engine-owned registry of per-view transports. Each view's LiveSocket
// gets its own transport class (Phoenix constructs it with `new
// transport(url)`; the url is a dead label), so its socket, channel and
// transport form an isolated, fully stock stack; inbound frames are routed
// to the right one by topic.
export function createPopcornTransports(
  pop: PopcornClient,
  hooks: Record<string, Hook>,
): PopcornTransports {
  const sinks = new Map<string, FrameSink>();

  const transportClassFor = (llvId: string) => {
    const topic = `lv:${llvId}`;

    return class PopcornTransport {
      readyState = 0; // CONNECTING
      onopen: () => void = () => {};
      onerror: (error: unknown) => void = () => {};
      onmessage: (event: { data: TransportFrame }) => void = () => {};
      onclose: (event: { code: number; wasClean: boolean }) => void = () => {};

      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      constructor(_endpointURL: string, _protocols?: unknown) {
        sinks.set(topic, this);
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

        // The call only acknowledges transport-level acceptance; the
        // channel's reply comes back through the push pipe as a phx_reply
        // frame, matched to this frame by ref. Rejected frames are acked
        // with an error so their Push fails fast instead of timing out.
        pop
          .call({ action: "transport_frame", id, frame }, { suppressErrorLog: true })
          .then((result) => {
            if (!result.ok) this.ack(frame, "error", result.error);
          });
      }

      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      close(_code?: number, _reason?: string): void {
        this.readyState = 3; // CLOSED
        if (sinks.get(topic) === this) sinks.delete(topic);
        queueMicrotask(() => this.onclose({ code: 1000, wasClean: true }));
      }
    };
  };

  return {
    route(frame: TransportFrame): void {
      sinks.get(frame.topic)?.inject(frame);
    },

    newSocket(llvId: string): LLVSocket {
      // Socket-level options (transport, encode/decode, timeout) are
      // forwarded by LiveSocket but absent from its published options type
      // — pass through a variable to skip the excess-property check.
      const opts = {
        transport: transportClassFor(llvId),
        timeout: JOIN_TIMEOUT_MS,
        encode: (payload: unknown, callback: (encoded: unknown) => void) => callback(payload),
        decode: (rawPayload: unknown, callback: (decoded: unknown) => void) => callback(rawPayload),
        hooks,
      };
      return new LiveSocket("/llv-popcorn", Socket, opts) as unknown as LLVSocket;
    },
  };
}
