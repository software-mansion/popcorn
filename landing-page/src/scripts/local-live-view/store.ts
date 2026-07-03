import { atom } from "nanostores";

export interface LogEntry {
  id: number;
  event: string;
  result: string;
}

export interface PresentationPayload {
  block: string | null;
  event: string;
  assigns: Record<string, unknown>;
}

// Animation step: 0=phx-click, 1=handle_event, 2=update assigns, 3=re-render, null=idle
export const $step = atom<number | null>(null);

// Current assigns from the LiveView
export const $assigns = atom<Record<string, unknown>>({});

// Keys that changed in the last update (for flash animation)
export const $flashKeys = atom<Set<string>>(new Set());

// Event log entries (newest first, max 6)
export const $log = atom<LogEntry[]>([]);

export function pushLog(entry: LogEntry) {
  $log.set([entry, ...$log.get()].slice(0, 6));
}
