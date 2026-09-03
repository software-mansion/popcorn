type CreateModuleFn<Mod> = (overrides?: Partial<Mod>) => Promise<Mod>;

/**
 * A message value. JavaScript sends use these BEAM conversions:
 *
 * - Strings become UTF-8 binaries. Booleans become `true` and `false` atoms.
 * - Integers become integers. Other finite numbers become floats.
 * - Arrays become lists.
 * - Plain objects become maps with binary string keys.
 * - `null` and `undefined` become the `nil` atom.
 * - `atom()` and `tuple()` create atoms and tuples. PIDs retain their BEAM identity.
 *
 * Cycles, class instances, functions, symbols, bigints, unsafe integers, and non-finite numbers cause `bridge:unserializable`.
 */
export type AnyValue = unknown;

declare const pidBrand: unique symbol;
/**
 * An opaque BEAM process identifier received from the VM.
 *
 * Valid only with the Popcorn instance and boot that produced it.
 */
export type Pid = { readonly [pidBrand]: true };

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
  | { type: "otp:tracked-value-delete"; payload: number };

export type RunJsRequest = {
  code: string;
  args: AnyValue;
  replyTo: Uint8Array;
  return: "value" | "ref";
};

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
