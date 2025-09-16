import { create } from "zustand";
import { hashPathname } from "../utils/storage";

type CodeEditorStore = {
  defaultCode: string;
  code: string;
  stdoutResult: string[];
  setStdoutResult: (stdoutResult: string) => void;
  resetStdoutResult: () => void;
  setCode: (code: string) => void;
  pathHash: string | null;
  setDefaultCode: (code: string) => void;
  resetCodeToDefault: () => void;
  setPathHash: (path: string) => void;
  getCodeFromStorage: () => void;
};

export const useCodeEditorStore = create<CodeEditorStore>((set, get) => ({
  defaultCode: "",
  code: "",
  stdoutResult: [],
  pathHash: null,

  setCode: (code: string) =>
    set((state) => {
      if (state.pathHash) {
        localStorage.setItem(`code-${state.pathHash}`, code);
      }

      return { code: code };
    }),
  setPathHash: (path: string) => set({ pathHash: hashPathname(path) }),
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
  setStdoutResult: (stdoutResult: string) =>
    set((state) => ({
      stdoutResult: [...state.stdoutResult, stdoutResult]
    })),
  resetStdoutResult: () => set({ stdoutResult: [] })
}));
