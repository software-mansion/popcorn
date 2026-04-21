export type EmscriptenFS = {
  mkdir: (path: string) => void;
  writeFile: (path: string, data: Uint8Array) => void;
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
  _malloc: (size: number) => number;
  _free: (ptr: number) => void;
  onBeamMessage?: (text: string) => Promise<void>;
  onError?: (text: string) => Promise<void>;
  addRunDependency: (id: string) => void;
  removeRunDependency: (id: string) => void;
};
