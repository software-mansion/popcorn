import XXH from "xxhashjs";

const SEED = 0x42;
const H32 = XXH.h32(SEED);
const H64 = XXH.h64(SEED);

export function hash32(pathname: string): string {
  return H32.update(pathname).digest().toString(16);
}

export function hash64(code: string): string {
  return H64.update(code).digest().toString(16);
}

function withLocalStorageErrorHandling<T>(operation: () => T) {
  try {
    return operation();
  } catch (error) {
    console.warn("Failed to access localStorage:", error);
    return null;
  }
}

export function getLocalStorageItem(key: string): string | null {
  return withLocalStorageErrorHandling(() => localStorage.getItem(key));
}

export function setLocalStorageItem(key: string, value: string): void {
  withLocalStorageErrorHandling(() => localStorage.setItem(key, value));
}

export function removeLocalStorageItem(key: string): void {
  withLocalStorageErrorHandling(() => localStorage.removeItem(key));
}

export function clearLocalStorage(): void {
  withLocalStorageErrorHandling(() => localStorage.clear());
}
