export type VirtualAddress = string;

export type VirtualNetworkCommand =
  | { version: 1; vmId: string; socketId: number; operation: "bind_udp"; address: VirtualAddress; port: number; requestId: number }
  | { version: 1; vmId: string; socketId: number; operation: "listen_tcp"; address: VirtualAddress; port: number; backlog: number; requestId: number }
  | { version: 1; vmId: string; socketId: number; operation: "connect_tcp"; address: VirtualAddress; port: number; requestId: number }
  | { version: 1; vmId: string; socketId: number; operation: "accept_tcp"; acceptedSocketId: number; requestId: number }
  | { version: 1; vmId: string; socketId: number; operation: "close"; direction: "read" | "write" | "read_write" }
  | { version: 1; vmId: string; socketId: number; operation: "tcp_data"; bytes: Uint8Array<ArrayBuffer> }
  | { version: 1; vmId: string; socketId: number; operation: "udp_data"; address: VirtualAddress; port: number; bytes: Uint8Array<ArrayBuffer> };

export type VirtualNetworkEvent =
  | { version: 1; operation: "ok"; requestId: number; address: VirtualAddress; port: number }
  | { version: 1; operation: "error"; requestId: number; reason: VirtualNetworkError }
  | { version: 1; operation: "tcp_connected"; requestId: number; address: VirtualAddress; port: number; localAddress: VirtualAddress; localPort: number }
  | { version: 1; operation: "tcp_accepted"; requestId: number; socketId: number; address: VirtualAddress; port: number }
  | { version: 1; operation: "tcp_data"; socketId: number; bytes: Uint8Array<ArrayBuffer> }
  | { version: 1; operation: "tcp_closed"; socketId: number; direction: "read" | "write" | "read_write"; reason?: VirtualNetworkError }
  | { version: 1; operation: "udp_data"; socketId: number; address: VirtualAddress; port: number; bytes: Uint8Array<ArrayBuffer> };

export type VirtualNetworkError =
  | "eaddrinuse"
  | "econnrefused"
  | "econnreset"
  | "enobufs"
  | "enoent";

type Endpoint = { vmId: string; socketId: number };
export type VirtualNetworkDelivery = { vmId: string; event: VirtualNetworkEvent };
type Vm = { address: VirtualAddress; hostname: string };
type Listener = {
  endpoint: Endpoint;
  address: VirtualAddress;
  port: number;
  backlog: number;
  pending: PendingConnection[];
  accepts: Array<{ socketId: number; requestId: number }>;
};
type PendingConnection = {
  client: Endpoint;
  clientAddress: VirtualAddress;
  clientPort: number;
  queued: Uint8Array<ArrayBuffer>[];
  queuedBytes: number;
};

const EPHEMERAL_START = 49_152;
const EPHEMERAL_END = 65_535;
const DEFAULT_QUEUE_LIMIT = 1024 * 1024;

export class VirtualNetworkBroker {
  private readonly vms = new Map<string, Vm>();
  private readonly listeners = new Map<string, Listener>();
  private readonly udpBindings = new Map<string, Endpoint>();
  private readonly peers = new Map<string, Endpoint>();
  private readonly closedWrites = new Set<string>();
  private readonly pendingClients = new Map<string, PendingConnection>();
  private nextAddress = 1;
  private nextPort = EPHEMERAL_START;

  public constructor(private readonly queueLimitBytes = DEFAULT_QUEUE_LIMIT) {}

  public registerVm(vmId: string): Vm {
    const existing = this.vms.get(vmId);
    if (existing !== undefined) return existing;
    const value = {
      address: `10.0.${Math.floor(this.nextAddress / 254)}.${(this.nextAddress % 254) + 1}`,
      hostname: `vm-${this.nextAddress}`,
    };
    this.nextAddress += 1;
    this.vms.set(vmId, value);
    return value;
  }

  public registerJs(vmId: string, id: number): Vm {
    const value = { address: `10.255.0.${id}`, hostname: `javascript-${id}` };
    this.vms.set(vmId, value);
    return value;
  }

  public unregisterVm(vmId: string): VirtualNetworkDelivery[] {
    const deliveries: VirtualNetworkDelivery[] = [];
    for (const listener of Array.from(this.listeners.values())) {
      if (listener.endpoint.vmId === vmId) {
        this.listeners.delete(bindingKey(listener.address, listener.port));
        for (const pending of listener.pending) {
          deliveries.push(closed(pending.client, "read_write", "econnreset"));
          this.pendingClients.delete(endpointKey(pending.client));
        }
      } else {
        listener.pending = listener.pending.filter((pending) => pending.client.vmId !== vmId);
      }
    }
    for (const [key, endpoint] of Array.from(this.udpBindings)) {
      if (endpoint.vmId === vmId) this.udpBindings.delete(key);
    }
    for (const [key, peer] of Array.from(this.peers)) {
      if (!this.peers.has(key)) continue;
      const endpoint = parseEndpointKey(key);
      if (endpoint.vmId !== vmId && peer.vmId !== vmId) continue;
      this.peers.delete(key);
      this.peers.delete(endpointKey(peer));
      this.closedWrites.delete(key);
      this.closedWrites.delete(endpointKey(peer));
      const survivor = endpoint.vmId === vmId ? peer : endpoint;
      if (survivor.vmId !== vmId) deliveries.push(closed(survivor, "read_write", "econnreset"));
    }
    for (const [key, pending] of Array.from(this.pendingClients)) {
      if (pending.client.vmId === vmId) this.pendingClients.delete(key);
    }
    this.vms.delete(vmId);
    return deliveries;
  }

  public command(command: VirtualNetworkCommand): VirtualNetworkDelivery[] {
    const vm = this.vms.get(command.vmId);
    if (vm === undefined) return requestError(command, "enoent");
    switch (command.operation) {
      case "bind_udp": return this.bindUdp(command, vm);
      case "listen_tcp": return this.listenTcp(command, vm);
      case "connect_tcp": return this.connectTcp(command, vm);
      case "accept_tcp": return this.acceptTcp(command);
      case "tcp_data": return this.tcpData(command);
      case "udp_data": return this.udpData(command, vm);
      case "close": return this.close(command);
    }
  }

  private bindUdp(command: Extract<VirtualNetworkCommand, { operation: "bind_udp" }>, vm: Vm): VirtualNetworkDelivery[] {
    const address = bindAddress(command.address, vm.address);
    const port = command.port === 0 ? this.allocatePort(address) : command.port;
    const key = bindingKey(address, port);
    if (this.udpBindings.has(key)) return requestError(command, "eaddrinuse");
    this.udpBindings.set(key, command);
    return [reply(command.vmId, { version: 1, operation: "ok", requestId: command.requestId, address, port })];
  }

  private listenTcp(command: Extract<VirtualNetworkCommand, { operation: "listen_tcp" }>, vm: Vm): VirtualNetworkDelivery[] {
    const address = bindAddress(command.address, vm.address);
    const port = command.port === 0 ? this.allocatePort(address) : command.port;
    const key = bindingKey(address, port);
    if (this.listeners.has(key)) return requestError(command, "eaddrinuse");
    this.listeners.set(key, { endpoint: command, address, port, backlog: command.backlog, pending: [], accepts: [] });
    return [reply(command.vmId, { version: 1, operation: "ok", requestId: command.requestId, address, port })];
  }

  private connectTcp(command: Extract<VirtualNetworkCommand, { operation: "connect_tcp" }>, vm: Vm): VirtualNetworkDelivery[] {
    const address = resolveAddress(resolveHost(command.address, vm.address), this.vms);
    const listener = this.listeners.get(bindingKey(address, command.port));
    if (listener === undefined) return requestError(command, "econnrefused");
    if (listener.pending.length >= listener.backlog && listener.accepts.length === 0) return requestError(command, "econnrefused");
    const pending = { client: command, clientAddress: vm.address, clientPort: this.allocatePort(vm.address), queued: [], queuedBytes: 0 };
    this.pendingClients.set(endpointKey(command), pending);
    const deliveries = [reply(command.vmId, { version: 1, operation: "tcp_connected", requestId: command.requestId, address, port: command.port, localAddress: vm.address, localPort: pending.clientPort })];
    const accept = listener.accepts.shift();
    if (accept === undefined) listener.pending.push(pending);
    else deliveries.push(...this.finishAccept(listener, pending, accept));
    return deliveries;
  }

  private acceptTcp(command: Extract<VirtualNetworkCommand, { operation: "accept_tcp" }>): VirtualNetworkDelivery[] {
    const listener = Array.from(this.listeners.values()).find((value) => sameEndpoint(value.endpoint, command));
    if (listener === undefined) return requestError(command, "enoent");
    const accept = { socketId: command.acceptedSocketId, requestId: command.requestId };
    const pending = listener.pending.shift();
    if (pending === undefined) {
      listener.accepts.push(accept);
      return [];
    }
    return this.finishAccept(listener, pending, accept);
  }

  private finishAccept(listener: Listener, pending: PendingConnection, accept: { socketId: number; requestId: number }): VirtualNetworkDelivery[] {
    const server = { vmId: listener.endpoint.vmId, socketId: accept.socketId };
    this.pendingClients.delete(endpointKey(pending.client));
    this.peers.set(endpointKey(pending.client), server);
    this.peers.set(endpointKey(server), pending.client);
    const deliveries = [reply(server.vmId, { version: 1, operation: "tcp_accepted", requestId: accept.requestId, socketId: server.socketId, address: pending.clientAddress, port: pending.clientPort })];
    for (const bytes of pending.queued) deliveries.push(reply(server.vmId, { version: 1, operation: "tcp_data", socketId: server.socketId, bytes }));
    return deliveries;
  }

  private tcpData(command: Extract<VirtualNetworkCommand, { operation: "tcp_data" }>): VirtualNetworkDelivery[] {
    if (this.closedWrites.has(endpointKey(command))) return [closed(command, "write", "econnreset")];
    if (command.bytes.byteLength > this.queueLimitBytes) {
      return [closed(command, "read_write", "enobufs"),
              ...this.closeEndpoint(command, "enobufs")];
    }
    const peer = this.peers.get(endpointKey(command));
    if (peer !== undefined) return [reply(peer.vmId, { version: 1, operation: "tcp_data", socketId: peer.socketId, bytes: command.bytes })];
    const pending = this.pendingClients.get(endpointKey(command));
    if (pending === undefined) return [closed(command, "read_write", "econnreset")];
    if (pending.queuedBytes + command.bytes.byteLength > this.queueLimitBytes) return this.closeEndpoint(command, "enobufs");
    pending.queued.push(command.bytes);
    pending.queuedBytes += command.bytes.byteLength;
    return [];
  }

  private udpData(command: Extract<VirtualNetworkCommand, { operation: "udp_data" }>, vm: Vm): VirtualNetworkDelivery[] {
    if (command.bytes.byteLength > this.queueLimitBytes) return [];
    const address = resolveAddress(command.address, this.vms);
    const target = this.udpBindings.get(bindingKey(address, command.port));
    if (target === undefined) return [];
    const source = Array.from(this.udpBindings.entries()).find(([, endpoint]) => sameEndpoint(endpoint, command));
    if (source === undefined) return [];
    const [, sourcePortText] = source[0].split(":");
    return [reply(target.vmId, { version: 1, operation: "udp_data", socketId: target.socketId, address: vm.address, port: Number(sourcePortText), bytes: command.bytes })];
  }

  private close(command: Extract<VirtualNetworkCommand, { operation: "close" }>): VirtualNetworkDelivery[] {
    for (const [key, listener] of this.listeners) {
      if (sameEndpoint(listener.endpoint, command)) this.listeners.delete(key);
      else listener.accepts = listener.accepts.filter((accept) =>
        !(listener.endpoint.vmId === command.vmId && accept.socketId === command.socketId));
    }
    for (const [key, endpoint] of this.udpBindings) if (sameEndpoint(endpoint, command)) this.udpBindings.delete(key);
    if (command.direction === "write") {
      const peer = this.peers.get(endpointKey(command));
      if (peer === undefined) return [];
      this.closedWrites.add(endpointKey(command));
      return [closed(peer, "read")];
    }
    return this.closeEndpoint(command);
  }

  private closeEndpoint(endpoint: Endpoint, reason?: VirtualNetworkError): VirtualNetworkDelivery[] {
    const peer = this.peers.get(endpointKey(endpoint));
    this.peers.delete(endpointKey(endpoint));
    this.closedWrites.delete(endpointKey(endpoint));
    this.pendingClients.delete(endpointKey(endpoint));
    if (peer === undefined) return [];
    this.peers.delete(endpointKey(peer));
    this.closedWrites.delete(endpointKey(peer));
    return [closed(peer, "read_write", reason)];
  }

  private allocatePort(address: string): number {
    for (let count = 0; count <= EPHEMERAL_END - EPHEMERAL_START; count += 1) {
      const port = this.nextPort;
      this.nextPort = port === EPHEMERAL_END ? EPHEMERAL_START : port + 1;
      const key = bindingKey(address, port);
      if (!this.listeners.has(key) && !this.udpBindings.has(key)) return port;
    }
    throw new Error("virtual ephemeral ports exhausted");
  }
}

function bindAddress(address: string, ownAddress: string): string { return address === "0.0.0.0" ? ownAddress : resolveHost(address, ownAddress); }
function resolveHost(address: string, ownAddress: string): string { return address === "localhost" || address === "127.0.0.1" ? ownAddress : address; }
function resolveAddress(address: string, vms: Map<string, Vm>): string {
  for (const vm of vms.values()) if (vm.hostname === address) return vm.address;
  return address;
}
function bindingKey(address: string, port: number): string { return `${address}:${port}`; }
function endpointKey(endpoint: Endpoint): string { return `${endpoint.vmId}\0${endpoint.socketId}`; }
function parseEndpointKey(key: string): Endpoint { const [vmId, socketId] = key.split("\0"); return { vmId, socketId: Number(socketId) }; }
function sameEndpoint(left: Endpoint, right: Endpoint): boolean { return left.vmId === right.vmId && left.socketId === right.socketId; }
function reply(vmId: string, event: VirtualNetworkEvent): VirtualNetworkDelivery { return { vmId, event }; }
function closed(endpoint: Endpoint, direction: "read" | "write" | "read_write", reason?: VirtualNetworkError): VirtualNetworkDelivery { return reply(endpoint.vmId, { version: 1, operation: "tcp_closed", socketId: endpoint.socketId, direction, reason }); }
function requestError(command: { vmId: string; requestId?: number }, reason: VirtualNetworkError): VirtualNetworkDelivery[] { return command.requestId === undefined ? [] : [reply(command.vmId, { version: 1, operation: "error", requestId: command.requestId, reason })]; }
