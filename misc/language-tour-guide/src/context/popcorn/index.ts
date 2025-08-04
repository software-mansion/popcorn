import { createContext } from "react";

interface InitParams {
  container?: HTMLElement;
  bundlePath?: string;
  onStderr?: (text: string) => void;
  onStdout?: (text: string) => void;
  heartbeatTimeoutMs?: number;
  debug?: boolean;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type AnySerializable = any;

export interface CastOptions {
  process: string;
}

export interface CallOptions {
  process: string;
  timeoutMs: number;
}

interface PopcornCallResult {
  data: AnySerializable;
  durationMs: number;
  error?: AnySerializable;
}

export interface Popcorn {
  deinit(): void;
  call(args: AnySerializable, options: CallOptions): Promise<PopcornCallResult>;
  cast(args: AnySerializable, options: CastOptions): void;
}

interface PopcornStatic {
  init(options: InitParams): Promise<Popcorn>;
}

declare global {
  interface Window {
    Popcorn: PopcornStatic;
  }
}

export interface PopcornContextValue {
  instance: Popcorn | null;
}

export const PopcornContext = createContext<PopcornContextValue | undefined>(
  undefined
);
