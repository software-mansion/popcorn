declare var Popcorn: Popcorn;
declare var terminal: object;

type InitOpts = {
  bundlePaths?: string[];
  wasmDir?: string;
  debug?: boolean;
  onStdout?: (text: string) => void;
  onStderr?: (text: string) => void;
  heartbeatTimeoutMs?: number;
};

type Popcorn = {
  init(opts: InitOpts): Promise<Popcorn>;
};

interface ImportMetaEnv {
  readonly PROD: boolean;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
