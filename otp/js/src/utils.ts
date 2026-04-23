import { DEBUG } from "./globals";
import type { EmscriptenFS } from "./types";

export function ensureDir(FS: EmscriptenFS, path: string): void {
  const parts = path.split("/").filter(Boolean);
  let current = "";
  for (const part of parts) {
    current += `/${part}`;
    try {
      FS.mkdir(current);
    } catch {
      // Directory already exists — ignore.
    }
  }
}

export function dirname(path: string): string {
  const idx = path.lastIndexOf("/");
  if (idx <= 0) return "/";
  return path.slice(0, idx);
}

export async function fetchBinary(url: string): Promise<Uint8Array | null> {
  const response = await fetch(url);
  if (response.ok !== true) return null;
  return new Uint8Array(await response.arrayBuffer());
}

export async function fetchJson<T>(url: string): Promise<T | null> {
  const response = await fetch(url);
  if (response.ok !== true) return null;
  try {
    return await response.json();
  } catch {
    return null;
  }
}

export function assert(ok: boolean, msg?: string): asserts ok {
  if (!DEBUG) return;
  if (!ok) throw new Error(msg === undefined ? "assert" : "assert: ${msg}");
}

export function check(ok: boolean, msg: string): asserts ok {
  if (!ok) throw new Error(msg);
}

export function unreachable(): never {
  check(false, "unreachable");
}
