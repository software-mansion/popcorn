type CreateModuleFn<Mod> = (overrides?: Partial<Mod>) => Promise<Mod>;

export type AnyValue = unknown;

declare const pidBrand: unique symbol;
export type Pid = { readonly [pidBrand]: true };

/** Internal wire target. The public `send` accepts `string | Pid` instead. */
export type BeamTarget = { name: string } | { pid: Uint8Array };

export type BeamSendPayload = {
  target: BeamTarget;
  etf: Uint8Array<ArrayBuffer>;
};

export type BeamBootOptions = {
  otpAssetsRoot: string;
  emulatorArgs?: string[];
  extraArgs?: string[];
  env?: Record<string, string>;
  ttySize: TtySize;
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
  | { type: "otp:tracked-value-delete"; payload: number }
  | { type: "otp:network-command"; payload: VirtualNetworkWireMessage };

export type VirtualNetworkWireMessage = {
  metadata: string;
  bytes: Uint8Array<ArrayBuffer>;
};

export type RunJsRequest = {
  code: string;
  args: AnyValue;
  replyTo: Uint8Array;
  return: "value" | "ref";
};

export type OtpErrorPayload =
  | { kind: "abort"; data: string }
  | { kind: "error"; data: string }
  | { kind: "exit"; data: number };

export type TtySize = {
  columns: number;
  rows: number;
};

/** Emscripten Module interface (subset exposed after instantiation). */
export type EmscriptenModule = {
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
  onVirtualNetworkCommand?: (
    metadata: string,
    bytes: Uint8Array<ArrayBuffer>,
  ) => void;
  addRunDependency: (id: string) => void;
  removeRunDependency: (id: string) => void;
};
