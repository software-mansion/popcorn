type CreateModuleFn<Mod> = (overrides?: Partial<Mod>) => Promise<Mod>;

export type BeamBootOptions = {
  assetsUrl: string;
  searchPaths: string[];
  extraArgs: string[];
  createModule: CreateModuleFn<EmscriptenModule>;
  emit: (event: BeamEvent) => void;
};

export type BeamEvent =
  | { type: "otp:stdout"; payload: string }
  | { type: "otp:stderr"; payload: string }
  | { type: "otp:exit"; payload: number }
  | { type: "otp:abort"; payload: string }
  | { type: "otp:error"; payload: string }
  | { type: "popcorn:boot"; payload: BeamBootOptions }
  | { type: "popcorn:boot-end"; payload: void };

export type EmscriptenFS = {
  mkdir: (path: string) => void;
  writeFile: (path: string, data: Uint8Array) => void;
  readFile: (
    path: string,
    options?: { encoding?: string },
  ) => Uint8Array | string;
};

export type AnyValue = unknown;

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
  addRunDependency: (id: string) => void;
  removeRunDependency: (id: string) => void;
};
