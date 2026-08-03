import { err } from "./errors";
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

export function base64ToBytes(b64: string): Uint8Array {
  const binary = atob(b64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}

export function check(ok: boolean, msg?: string): asserts ok {
  if (!ok) throw err("internal:check", { detail: msg });
}

export function unreachable(): never {
  throw err("internal:unreachable", {});
}

export function objectWithKeys<K extends string>(
  value: unknown,
  keys: K[],
): null | Record<K, unknown> {
  if (value === null || typeof value !== "object") return null;
  if (value.constructor !== Object) return null;
  const hasAllKeys = keys.every((k) => Object.hasOwn(value, k));
  if (!hasAllKeys) return null;
  return value as Record<K, unknown>;
}
