import type { DistEndpoint, DistTransport } from "./types";

/**
 * Transport that you can pass as `distribution` option to multiple Popcorn
 * instances to connect them in a cluster.
 *
 * Works within a single page.
 * As in regular distribution, nodes must use the same cookie.
 *
 * @example
 * ```ts
 * const relay = createDistributionRelay();
 * const node = (name: string) => ({
 *   distribution: relay,
 *   beam: { extraArgs: ["-name", name, "-setcookie", "secret"] },
 * });
 * const a = await Popcorn.init(node("a@popcorn"));
 * const b = await Popcorn.init(node("b@popcorn"));
 * ```
 */
export function createDistributionRelay(): DistTransport {
  const nodes = new Map<string, DistEndpoint>();
  const connections = new Map<string, [DistEndpoint, DistEndpoint]>();

  const otherEnd = (endpoint: DistEndpoint, connection: string) => {
    const ends = connections.get(connection);
    if (ends === undefined) return undefined;
    return ends[0] === endpoint ? ends[1] : ends[0];
  };

  const disconnect = (endpoint: DistEndpoint, connection: string) => {
    otherEnd(endpoint, connection)?.deliver({ type: "disconnect", connection });
    connections.delete(connection);
  };

  return (endpoint) => (event) => {
    switch (event.type) {
      case "node_started":
        nodes.set(event.name, endpoint);
        return;
      case "node_stopped":
        if (nodes.get(event.name) === endpoint) nodes.delete(event.name);
        for (const [connection, ends] of connections) {
          if (ends.includes(endpoint)) disconnect(endpoint, connection);
        }
        return;
      case "connect": {
        const peer = nodes.get(event.to);
        if (peer === undefined) {
          // Refuses at once, rather than when the connection setup times out
          const { connection } = event;
          endpoint.deliver({ type: "disconnect", connection });
          return;
        }
        connections.set(event.connection, [endpoint, peer]);
        peer.deliver(event);
        return;
      }
      case "message":
        otherEnd(endpoint, event.connection)?.deliver(event);
        return;
      case "disconnect":
        disconnect(endpoint, event.connection);
        return;
    }
  };
}
