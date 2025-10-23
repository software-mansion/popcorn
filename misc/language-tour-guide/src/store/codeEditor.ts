import { create } from "zustand";
import { hash32 } from "../utils/storage";

type CodeEditorStore = {
  defaultCode: string;
  code: string;
  stdoutResult: string[];
  stderrResult: string[];
  setStdoutResult: (stdoutResult: string) => void;
  setStderrResult: (stderrResult: string) => void;
  resetStdoutResult: () => void;
  resetStderrResult: () => void;
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
  stdoutResult: [],
  stderrResult: [],
  pathHash: null,

  setCode: (code: string) =>
    set((state) => {
      const pathHashForStorage = state.pathHash;
      if (pathHashForStorage) {
        localStorage.setItem(`code-${pathHashForStorage}`, code);
      }

      return { code: code };
    }),
  setPathHash: (path: string) => set({ pathHash: hash32(path) }),
  getCodeFromStorage: () => {
    const { pathHash } = get();
    return localStorage.getItem(`code-${pathHash}`);
  },
  setDefaultCode: (defaultCode: string) => set({ defaultCode }),
  resetCodeToDefault: () =>
    set((state) => {
      localStorage.removeItem(`code-${state.pathHash}`);
      return { code: state.defaultCode };
    }),
  isCodeChanged: () => {
    const { code, defaultCode } = get();
    return code !== defaultCode;
  },
  setStdoutResult: (stdoutResult: string) =>
    set((state) => ({
      stdoutResult: [...state.stdoutResult, stdoutResult]
    })),
  setStderrResult: (stderrResult: string) =>
    set((state) => ({
      stderrResult: [...state.stderrResult, stderrResult]
    })),
  resetStdoutResult: () => set({ stdoutResult: [] }),
  resetStderrResult: () => set({ stderrResult: [] })
}));
