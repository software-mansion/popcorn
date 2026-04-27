import { PopcornInternalError } from "./internal-error";
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

export function isGzip(data: Uint8Array): boolean {
  return (
    data.length >= 3 &&
    data[0] === 0x1f &&
    data[1] === 0x8b &&
    data[2] === 0x08
  );
}

export async function decompressGzip(data: Uint8Array): Promise<Uint8Array> {
  const bytes = data.slice();
  const stream = new Blob([bytes])
    .stream()
    .pipeThrough(new DecompressionStream("gzip"));
  const buffer = await new Response(stream).arrayBuffer();
  return new Uint8Array(buffer);
}

export function check(ok: boolean, msg?: string): asserts ok {
  if (!ok) throw new PopcornInternalError("check", msg);
}

export function unreachable(): never {
  throw new PopcornInternalError("unreachable");
}

export function objectWithKeys<K extends string>(
  value: unknown,
  keys: K[],
): null | Record<K, unknown> {
  const isObject = value !== null && typeof value === "object";
  if (!isObject) return null;
  const hasAllKeys = keys.every((k) => Object.hasOwn(value, k));
  if (!hasAllKeys) return null;
  return value as Record<K, unknown>;
}
