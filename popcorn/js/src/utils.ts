export function dirname(path: string): string {
  const idx = path.lastIndexOf("/");
  if (idx <= 0) return "/";
  return path.slice(0, idx);
}

export async function fetchBinary(url: string): Promise<Uint8Array | null> {
  try {
    const response = await fetch(url);
    if (response.ok !== true) return null;
    return new Uint8Array(await response.arrayBuffer());
  } catch {
    return null;
  }
}

export async function fetchJson<T>(url: string): Promise<T | null> {
  try {
    const response = await fetch(url);
    if (response.ok !== true) return null;
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
  if (!ok) throw new Error(msg ?? "Check failed");
}

export function unreachable(_value?: never): never {
  throw new Error("Entered unreachable code");
}

export function objectWithKeys<K extends string>(
  value: unknown,
  keys: K[],
): null | Record<K, unknown> {
  if (value === null || typeof value !== "object") return null;
  if (Object.getPrototypeOf(value) !== Object.prototype) return null;
  const hasAllKeys = keys.every((k) => Object.hasOwn(value, k));
  if (!hasAllKeys) return null;
  return value as Record<K, unknown>;
}
