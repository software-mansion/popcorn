declare var Popcorn: Popcorn;
declare var terminal: object;

type InitOpts = {
  bundlePath?: string;
  wasmDir?: string;
  debug?: boolean;
  onStdout?: (text: string) => void;
  onStderr?: (text: string) => void;
  heartbeatTimeoutMs?: number;
};

type Popcorn = {
  init(opts: InitOpts): Promise<Popcorn>;
};
