type CreateModuleFn<Mod> = (overrides?: Partial<Mod>) => Promise<Mod>;

export type AnyValue = unknown;

export type BeamTarget = { name: string } | { pid: string };

export type BeamSendPayload = {
  target: BeamTarget;
  etf: Uint8Array<ArrayBuffer>;
};

export type BeamBootOptions = {
  manifestUrl: string;
  extraArgs?: string[];
  createModule: CreateModuleFn<EmscriptenModule>;
  emit: (event: BeamEvent) => void;
};

export type BeamEvent =
  | { type: "otp:stdout"; payload: string }
  | { type: "otp:stderr"; payload: string }
  | { type: "otp:error"; payload: OtpErrorPayload }
  | { type: "otp:message"; payload: AnyValue }
  | { type: "otp:run_js"; payload: RunJsRequest }
  | { type: "otp:tracked-value-delete"; payload: number };

export type RunJsRequest = {
  code: string;
  args: AnyValue;
  replyTo: string;
};

export type OtpErrorPayload =
  | { kind: "abort"; data: string }
  | { kind: "error"; data: string }
  | { kind: "exit"; data: number };

export type EmscriptenFS = {
  mkdir: (path: string) => void;
  writeFile: (path: string, data: Uint8Array) => void;
  readFile: (
    path: string,
    options?: { encoding?: string },
  ) => Uint8Array | string;
};

/** Emscripten Module interface (subset exposed after instantiation). */
export type EmscriptenModule = {
  ENV: Record<string, string>;
  FS: EmscriptenFS;
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
  onTrackedValueDelete?: (key: number) => void;
  addRunDependency: (id: string) => void;
  removeRunDependency: (id: string) => void;
};
