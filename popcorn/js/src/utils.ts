export function dirname(path: string): string {
  const idx = path.lastIndexOf("/");
  if (idx <= 0) return "/";
  return path.slice(0, idx);
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
