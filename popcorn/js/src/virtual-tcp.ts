import {
  VirtualNetworkBroker,
  type VirtualNetworkDelivery,
  type VirtualNetworkError,
  type VirtualNetworkEvent,
} from "./virtual-network";

type Dispatcher = (deliveries: VirtualNetworkDelivery[]) => void;

export class VirtualTcpError extends Error {
  public constructor(public readonly reason: VirtualNetworkError) {
    super(`virtual TCP: ${reason}`);
  }
}

export class VirtualTcpSocket {
  public readonly readable: ReadableStream<Uint8Array<ArrayBuffer>>;
  private controller!: ReadableStreamDefaultController<Uint8Array<ArrayBuffer>>;
  private readClosed = false;
  private writeClosed = false;

  public constructor(
    private readonly broker: VirtualNetworkBroker,
    private readonly dispatch: Dispatcher,
    private readonly vmId: string,
    private readonly socketId: number,
    public readonly localAddress: string,
    public readonly localPort: number,
    public readonly remoteAddress: string,
    public readonly remotePort: number,
    private readonly release: () => void,
  ) {
    this.readable = new ReadableStream({
      start: (controller) => { this.controller = controller; },
      cancel: () => { this.close(); },
    });
  }

  public async write(bytes: Uint8Array<ArrayBuffer>): Promise<void> {
    if (this.writeClosed) throw new VirtualTcpError("econnreset");
    const copy = bytes.slice();
    this.dispatch(this.broker.command({ version: 1, vmId: this.vmId, socketId: this.socketId, operation: "tcp_data", bytes: copy }));
  }

  public closeWrite(): void {
    if (this.writeClosed) return;
    this.writeClosed = true;
    this.dispatch(this.broker.command({ version: 1, vmId: this.vmId, socketId: this.socketId, operation: "close", direction: "write" }));
    this.releaseIfClosed();
  }

  public close(): void {
    if (this.readClosed && this.writeClosed) return;
    this.readClosed = true;
    this.writeClosed = true;
    this.dispatch(this.broker.command({ version: 1, vmId: this.vmId, socketId: this.socketId, operation: "close", direction: "read_write" }));
    this.release();
  }

  public receive(event: VirtualNetworkEvent): void {
    if (event.operation === "tcp_data") {
      this.controller.enqueue(event.bytes);
      return;
    }
    if (event.operation !== "tcp_closed") return;
    this.readClosed = true;
    if (event.reason === undefined) this.controller.close();
    else this.controller.error(new VirtualTcpError(event.reason));
    if (event.direction === "read_write") this.writeClosed = true;
    this.releaseIfClosed();
  }

  private releaseIfClosed(): void {
    if (this.readClosed && this.writeClosed) this.release();
  }
}

export class VirtualTcpClient {
  private readonly vmId: string;
  private readonly sockets = new Map<number, VirtualTcpSocket>();
  private readonly pending = new Map<number, { socketId: number; resolve: (socket: VirtualTcpSocket) => void; reject: (error: Error) => void }>();
  private socketSeq = 0;
  private requestSeq = 0;

  public constructor(
    private readonly broker: VirtualNetworkBroker,
    private readonly dispatchVm: Dispatcher,
    id: number,
  ) {
    this.vmId = `javascript-${id}`;
    this.broker.registerJs(this.vmId, id);
  }

  public connect(host: string, port: number, options: { signal?: AbortSignal } = {}): Promise<VirtualTcpSocket> {
    const socketId = ++this.socketSeq;
    const requestId = ++this.requestSeq;
    const promise = new Promise<VirtualTcpSocket>((resolve, reject) => {
      this.pending.set(requestId, { socketId, resolve, reject });
      this.dispatch(this.broker.command({ version: 1, vmId: this.vmId, socketId, operation: "connect_tcp", address: host, port, requestId }));
    });
    options.signal?.addEventListener("abort", () => this.abort(socketId, requestId, options.signal?.reason), { once: true });
    return promise;
  }

  public receive(delivery: VirtualNetworkDelivery): boolean {
    if (delivery.vmId !== this.vmId) return false;
    this.dispatch([delivery]);
    return true;
  }

  private abort(socketId: number, requestId: number, reason: unknown): void {
    const pending = this.pending.get(requestId);
    if (pending === undefined) return;
    this.pending.delete(requestId);
    this.dispatch(this.broker.command({ version: 1, vmId: this.vmId, socketId, operation: "close", direction: "read_write" }));
    pending.reject(reason instanceof Error ? reason : new DOMException("Aborted", "AbortError"));
  }

  private dispatch(deliveries: VirtualNetworkDelivery[]): void {
    const vmDeliveries: VirtualNetworkDelivery[] = [];
    for (const delivery of deliveries) {
      if (delivery.vmId !== this.vmId) {
        vmDeliveries.push(delivery);
        continue;
      }
      const event = delivery.event;
      if (event.operation === "tcp_connected") {
        const pending = this.pending.get(event.requestId);
        if (pending === undefined) continue;
        this.pending.delete(event.requestId);
        const socketId = pending.socketId;
        const socket = new VirtualTcpSocket(this.broker, (next) => this.dispatch(next), this.vmId, socketId, event.localAddress, event.localPort, event.address, event.port, () => this.sockets.delete(socketId));
        this.sockets.set(socketId, socket);
        pending.resolve(socket);
      } else if (event.operation === "error") {
        const pending = this.pending.get(event.requestId);
        if (pending === undefined) continue;
        this.pending.delete(event.requestId);
        pending.reject(new VirtualTcpError(event.reason));
      } else if (event.operation === "tcp_data" || event.operation === "tcp_closed") {
        this.sockets.get(event.socketId)?.receive(event);
      }
    }
    this.dispatchVm(vmDeliveries);
  }
}
