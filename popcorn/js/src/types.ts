type CreateModuleFn<Mod> = (overrides?: Partial<Mod>) => Promise<Mod>;

/**
 * A value exchanged with BEAM. Containers convert their contents recursively.
 *
 * See [Sharing values across languages](values.html) for supported types and conversions.
 */
export type AnyValue = unknown;

declare const pidBrand: unique symbol;
/**
 * An opaque BEAM process identifier received from the VM.
 *
 * Valid only with the Popcorn instance and boot that produced it.
 */
export type Pid = { readonly [pidBrand]: true };

/**
 * An event of an Erlang distribution connection between two nodes:
 *
 * - `connect` - opens a new connection between two nodes,
 * - `message` - piece of data for the other end of the connection,
 * - `disconnect` - removes the connection
 *
 * All these events must be forwarded to the other end of the connection.
 * If they can no longer be forwarded, the connection should be removed
 * by sending a `disconnect` event to each end, if possible.
 */
export type DistConnectionEvent =
  | { type: "connect"; connection: string; from: string; to: string }
  | { type: "message"; connection: string; data: Uint8Array }
  | { type: "disconnect"; connection: string };

/**
 * Indicates Erlang distribution node change:
 * - `node_started` - new node is ready to send/receive `DistConnectionEvent`s,
 * - `node_stopped` - node is terminated or left the cluster - all its connections
 *                    should be disconnected
 */
export type DistNodeEvent = {
  type: "node_started" | "node_stopped";
  name: string;
};

export type DistEndpoint = {
  /**
   * Delivers a {@link DistConnectionEvent} from the other end of the
   * connection, or `disconnect` when the other end is gone.
   *
   * Code that can call it can connect to the VM, only the distribution
   * cookie protects it. It stops delivering when the VM shuts down.
   */
  readonly deliver: (event: DistConnectionEvent) => void;
};

/**
 * Accepts {@link DistEndpoint} and returns function - a handler for
 * distribution events sent by the VM. Popcorn calls it each time the VM
 * boots, with a new endpoint.
 *
 * The events are the following:
 * - {@link DistNodeEvent} - tells when a node starts or stops,
 * - {@link DistConnectionEvent} - should be forwarded between nodes via their
 *   {@link DistEndpoint}s.
 *
 * See documentation for {@link DistEndpoint}, {@link DistNodeEvent}
 * and {@link DistConnectionEvent} for details.
 */
export type DistTransport = (
  endpoint: DistEndpoint,
) => (event: DistNodeEvent | DistConnectionEvent) => void;

/** Internal wire target. The public `send` accepts `string | Pid` instead. */
export type BeamTarget = { name: string } | { pid: Uint8Array };

export type BeamSendPayload = {
  target: BeamTarget;
  etf: Uint8Array<ArrayBuffer>;
};

export type BeamBootOptions = {
  otpAssetsRoot: string;
  /**
   * Emulator flags before `--`.
   *
   * Overrides the default scheduler flags.
   * Use `schedulers()` to set thread counts.
   */
  emulatorArgs?: string[];
  /** Erlang arguments after the boot arguments, such as `-eval` expressions. */
  extraArgs?: string[];
  /** VM environment variables. */
  env?: Record<string, string>;
  /** Resolved maximum Wasm linear memory in bytes. */
  maxMemoryBytes?: number;
  ttySize: TtySize;
  noshell: boolean;
  createModule: CreateModuleFn<EmscriptenModule>;
  emit: (event: BeamEvent) => void;
};

export type BeamEvent =
  | { type: "otp:stdout"; payload: Uint8Array }
  | { type: "otp:stderr"; payload: Uint8Array }
  | { type: "otp:stdin-consumed"; payload: number }
  | { type: "otp:error"; payload: OtpErrorPayload }
  | { type: "otp:message"; payload: AnyValue }
  | { type: "otp:run_js"; payload: RunJsRequest }
  | { type: "otp:await-ready"; payload: ReadyRequest }
  | { type: "otp:tracked-value-delete"; payload: number };

export type RunJsRequest = {
  code: string;
  args: AnyValue;
  replyTo: Uint8Array;
  return: "value" | "ref";
};

export type ReadyRequest = { replyTo: Uint8Array };

/**
 * VM shutdown notification.
 *
 * An `exit` carries a status code, including zero for a normal exit.
 */
export type OtpErrorPayload =
  | { kind: "abort"; data: string }
  | { kind: "error"; data: string }
  | { kind: "exit"; data: number };

/** Terminal dimensions in character cells, from 1 to 65,535 per dimension. */
export type TtySize = {
  columns: number;
  rows: number;
};

/** Emscripten Module interface (subset exposed after instantiation). */
export type EmscriptenModule = {
  wasmMemory?: WebAssembly.Memory;
  ENV: Record<string, string>;
  FS_mkdirTree: (path: string) => void;
  FS_createDataFile: (
    parent: string,
    name: string | null,
    data: Uint8Array,
    canRead: boolean,
    canWrite: boolean,
    canOwn: boolean,
  ) => void;
  HEAPU8: Uint8Array;
  ccall: (
    ident: string,
    returnType: string | null,
    argTypes: string[],
    args: unknown[],
  ) => AnyValue;
  print: (text: string) => void;
  printErr: (text: string) => void;
  onExit: (code: number) => void;
  onAbort: (text: string) => void;
  arguments: string[];
  preRun: ((mod: EmscriptenModule) => void)[];
  _malloc: (size: number) => number;
  _free: (ptr: number) => void;
  onBeamMessage?: (text: string) => void | Promise<void>;
  onError?: (text: string) => void | Promise<void>;
  onStdinConsumed?: (size: number) => void;
  onTrackedValueDelete?: (key: number) => void;
  onTtyChunk?: (fd: number, chunk: Uint8Array) => void;
  addRunDependency: (id: string) => void;
  removeRunDependency: (id: string) => void;
};
