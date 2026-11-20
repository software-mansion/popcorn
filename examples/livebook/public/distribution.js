// Erlang distribution between the VMs of all the example's tabs, as Popcorn's
// createDistributionRelay() connects those of one page. Each tab connects its
// own VMs, and registers them with distribution-worker.js, a SharedWorker. For
// a connection to another tab's VM, the worker gives the two tabs the ends of a
// MessageChannel, so only they see its data: other code of the origin can't
// listen in, and can connect only with the VM's cookie. Node names must differ
// across tabs.
//
// Each tab holds the lock `livebook-tab:<id>` while it's open, see index.html.
// A tab waits for the locks of the tabs it's connected to, which the browser
// releases when they close, even when they crash, and then disconnects them.
//
// Without SharedWorker, which some browsers lack, or when its script fails, a
// tab's VMs connect only to each other.

export function createTabRelay(tab) {
  let broker = null;
  try {
    const worker = new SharedWorker("/distribution-worker.js", { name: "livebook-distribution" });
    worker.addEventListener("error", () => {
      console.warn("[distribution] The SharedWorker failed, so the tab's VMs connect only to each other");
      broker = null;
      for (const connection of [...pending.keys()]) refuse(connection);
    });
    broker = worker.port;
  } catch (error) {
    console.warn(`[distribution] No SharedWorker, so the tab's VMs connect only to each other: ${error}`);
  }
  // This tab's nodes' endpoints
  const nodes = new Map();
  // Connections to their two ends: endpoints, or ports to other tabs
  const connections = new Map();
  // Connections to other tabs, to those tabs' ids
  const tabs = new Map();
  // Connections to other tabs that the worker hasn't set up yet, with their
  // endpoints and the events for them meanwhile
  const pending = new Map();
  const watched = new Set();

  const deliver = (end, event) => {
    if (!(end instanceof MessagePort)) {
      end.deliver(event);
    } else if (event.type === "message") {
      // A copy of its own, rather than of the whole buffer it may be a view of
      const data = new Uint8Array(event.data);
      end.postMessage({ type: "message", data }, [data.buffer]);
    } else {
      end.postMessage({ type: event.type });
    }
  };

  const otherEnd = (end, connection) => {
    const ends = connections.get(connection);
    if (ends === undefined) return null;
    return ends[0] === end ? ends[1] : ends[0];
  };

  // Closes the port of a connection to another tab when the other tab ends
  // it, after its last message, which closing it first could lose
  const disconnect = (end, connection) => {
    const other = otherEnd(end, connection);
    connections.delete(connection);
    tabs.delete(connection);
    if (other !== null) deliver(other, { type: "disconnect", connection });
    if (end instanceof MessagePort) end.close();
  };

  // The other end of a connection to another tab. Its messages are for this
  // connection only, whatever they say.
  const attach = (port, connection, otherTab) => {
    tabs.set(connection, otherTab);
    watch(otherTab);
    port.onmessage = ({ data }) => {
      if (!connections.has(connection)) return;
      if (data?.type === "message" && data.data instanceof Uint8Array) {
        otherEnd(port, connection).deliver({ type: "message", connection, data: data.data });
      } else if (data?.type === "disconnect") {
        disconnect(port, connection);
      }
    };
  };

  const watch = (other) => {
    if (watched.has(other)) return;
    watched.add(other);
    // As if the other tab ended its connections
    navigator.locks.request(`livebook-tab:${other}`, () => {
      for (const [connection, otherTab] of tabs) {
        const port = connections.get(connection)?.find((end) => end instanceof MessagePort);
        if (otherTab === other && port !== undefined) disconnect(port, connection);
      }
    });
  };

  // From this tab's VMs
  // Refuses at once, rather than when the connection setup times out
  const refuse = (connection) => {
    pending.get(connection)?.endpoint.deliver({ type: "disconnect", connection });
    pending.delete(connection);
  };

  const route = (endpoint, event) => {
    const { connection } = event;
    switch (event.type) {
      case "connect": {
        const peer = nodes.get(event.to);
        if (peer !== undefined) {
          connections.set(connection, [endpoint, peer]);
          peer.deliver(event);
        } else {
          pending.set(connection, { endpoint, events: [] });
          if (broker === null) refuse(connection);
          else broker.postMessage({ type: "connect", connection, from: event.from, to: event.to });
        }
        return;
      }
      case "message":
        if (pending.has(connection)) pending.get(connection).events.push(event);
        else if (connections.has(connection)) deliver(otherEnd(endpoint, connection), event);
        return;
      case "disconnect":
        // The worker's port, if it comes, finds no connection, see below
        if (!pending.delete(connection)) disconnect(endpoint, connection);
        return;
    }
  };

  const fromBroker = ({ data }) => {
    const { connection } = data;
    switch (data.type) {
      // To this tab's node from another tab's
      case "connect": {
        const peer = nodes.get(data.to);
        if (peer === undefined) {
          deliver(data.port, { type: "disconnect" });
          return;
        }
        connections.set(connection, [data.port, peer]);
        attach(data.port, connection, data.tab);
        peer.deliver({ type: "connect", connection, from: data.from, to: data.to });
        return;
      }
      // From this tab's node to another tab's
      case "connected": {
        const started = pending.get(connection);
        pending.delete(connection);
        if (started === undefined) {
          deliver(data.port, { type: "disconnect" });
          return;
        }
        connections.set(connection, [started.endpoint, data.port]);
        attach(data.port, connection, data.tab);
        for (const event of started.events) deliver(data.port, event);
        return;
      }
      case "refused":
        refuse(connection);
        return;
      case "taken":
        console.warn(`[distribution] ${data.name} is a node of another tab too`);
        return;
    }
  };
  if (broker !== null) {
    broker.onmessage = fromBroker;
    broker.postMessage({ type: "tab", tab });
  }

  return (endpoint) => (event) => {
    switch (event.type) {
      case "node_started":
        nodes.set(event.name, endpoint);
        broker?.postMessage({ type: "register", name: event.name });
        return;
      case "node_stopped":
        if (nodes.get(event.name) === endpoint) {
          nodes.delete(event.name);
          broker?.postMessage({ type: "unregister", name: event.name });
        }
        for (const [connection, ends] of connections) if (ends.includes(endpoint)) disconnect(endpoint, connection);
        for (const [connection, started] of pending) if (started.endpoint === endpoint) pending.delete(connection);
        return;
      default:
        route(endpoint, event);
    }
  };
}
