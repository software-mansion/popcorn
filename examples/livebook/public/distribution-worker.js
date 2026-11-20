// The SharedWorker of the example's tabs that connects their nodes, see
// distribution.js. It keeps which tab has which node, and gives two tabs the
// ends of a MessageChannel for each connection between their nodes. It doesn't
// tell anyone the names, or see the connections' data.

// Node names to their tabs: their ports here and their ids
const nodes = new Map();

onconnect = ({ ports: [port] }) => {
  let tab = null;

  port.onmessage = ({ data }) => {
    if (tab === null && data.type !== "tab") return;
    switch (data.type) {
      case "tab":
        if (tab !== null) return;
        tab = String(data.tab);
        // The browser releases the tab's lock when the tab closes, see index.html
        navigator.locks.request(`livebook-tab:${tab}`, () => {
          for (const [name, node] of nodes) if (node.port === port) nodes.delete(name);
          port.close();
        });
        return;
      case "register":
        if (!nodes.has(data.name)) nodes.set(data.name, { port, tab });
        else port.postMessage({ type: "taken", name: data.name });
        return;
      case "unregister":
        if (nodes.get(data.name)?.port === port) nodes.delete(data.name);
        return;
      case "connect": {
        const { connection, from, to } = data;
        const target = nodes.get(to);
        // Only between the tab's own node and another tab's
        if (nodes.get(from)?.port !== port || target === undefined || target.port === port) {
          port.postMessage({ type: "refused", connection });
          return;
        }
        const { port1, port2 } = new MessageChannel();
        target.port.postMessage({ type: "connect", connection, from, to, tab, port: port1 }, [port1]);
        port.postMessage({ type: "connected", connection, tab: target.tab, port: port2 }, [port2]);
        return;
      }
    }
  };
};
