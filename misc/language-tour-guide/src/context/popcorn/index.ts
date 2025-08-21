import { createContext } from "react";

type InitParams = {
  container?: HTMLElement;
  bundlePath?: string;
  onStderr?: (text: string) => void;
  onStdout?: (text: string) => void;
  wasmDir?: string;
  heartbeatTimeoutMs?: number;
  debug?: boolean;
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type AnySerializable = any;

export type CastOptions = {
  process: string;
};

export type CallOptions = {
  process?: string;
  timeoutMs?: number;
};

type PopcornCallResult = {
  data: AnySerializable;
  durationMs: number;
  error?: AnySerializable;
};

export type Popcorn = {
  deinit(): void;
  call(args: AnySerializable, options: CallOptions): Promise<PopcornCallResult>;
  cast(args: AnySerializable, options: CastOptions): void;
};

type PopcornStatic = {
  init(options: InitParams): Promise<Popcorn>;
};

declare global {
  interface Window {
    Popcorn: PopcornStatic;
  }
}

export type PopcornContextValue = {
  instance: Popcorn | null;
};

export const PopcornContext = createContext<PopcornContextValue | undefined>(
  undefined
);
