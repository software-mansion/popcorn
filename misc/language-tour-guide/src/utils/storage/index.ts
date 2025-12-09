import XXH from "xxhashjs";
import type { ExecutionState } from "../../store/editors";

const CONTENT_HASH_KEY = "defaultCodeHash";
const SEED = 0x42;
const H32 = XXH.h32(SEED);
export const H64 = XXH.h64(SEED);

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

const EDITOR_STORAGE_PREFIX = "editor-";

export type SavedEditorState = {
  code: string | null;
  state: ExecutionState;
};

export function getEditorStorageKey(editorId: string): string {
  return `${EDITOR_STORAGE_PREFIX}${editorId}`;
}

export function getSavedEditorState(editorId: string): SavedEditorState | null {
  const data = getLocalStorageItem(getEditorStorageKey(editorId));
  if (!data) return null;
  try {
    return JSON.parse(data) as SavedEditorState;
  } catch {
    return null;
  }
}

export function saveEditorState(
  editorId: string,
  state: SavedEditorState
): void {
  setLocalStorageItem(getEditorStorageKey(editorId), JSON.stringify(state));
}

export function removeSavedEditorState(editorId: string): void {
  removeLocalStorageItem(getEditorStorageKey(editorId));
}

export function syncContentHash(): void {
  const storedHash = getLocalStorageItem(CONTENT_HASH_KEY);

  if (storedHash !== __CONTENT_HASH__) {
    clearLocalStorage();
    setLocalStorageItem(CONTENT_HASH_KEY, __CONTENT_HASH__);
  }
}
