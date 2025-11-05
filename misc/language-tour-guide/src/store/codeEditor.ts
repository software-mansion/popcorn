import { create } from "zustand";
import {
  hash32,
  getLocalStorageItem,
  setLocalStorageItem,
  removeLocalStorageItem
} from "../utils/storage";

type CodeEditorStore = {
  defaultCode: string;
  code: string;
  setCode: (code: string) => void;
  pathHash: string | null;
  setDefaultCode: (code: string) => void;
  isCodeChanged: () => boolean;
  resetCodeToDefault: () => void;
  setPathHash: (path: string) => void;
  getCodeFromStorage: () => void;
};

export const useCodeEditorStore = create<CodeEditorStore>((set, get) => ({
  defaultCode: "",
  code: "",
  pathHash: null,

  setCode: (code: string) =>
    set((state) => {
      const pathHashForStorage = state.pathHash;
      if (pathHashForStorage) {
        setLocalStorageItem(`code-${pathHashForStorage}`, code);
      }

      return { code: code };
    }),
  setPathHash: (path: string) => set({ pathHash: hash32(path) }),
  getCodeFromStorage: () => {
    const { pathHash } = get();
    return getLocalStorageItem(`code-${pathHash}`);
  },
  setDefaultCode: (defaultCode: string) => set({ defaultCode }),
  resetCodeToDefault: () =>
    set((state) => {
      removeLocalStorageItem(`code-${state.pathHash}`);
      return { code: state.defaultCode };
    }),
  isCodeChanged: () => {
    const { code, defaultCode } = get();
    return code !== defaultCode;
  }
}));
