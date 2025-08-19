import { create } from "zustand";

type CodeEditorStore = {
  defaultCode: string;
  code: string;
  stdoutResult: string[];
  setStdoutResult: (stdoutResult: string) => void;
  resetStdoutResult: () => void;
  setCode: (code: string) => void;
  setDefaultCode: (code: string) => void;
  resetToDefault: () => void;
};

export const useCodeEditorStore = create<CodeEditorStore>((set) => ({
  defaultCode: "",
  code: "",
  stdoutResult: [],
  setCode: (code: string) => set({ code }),
  setDefaultCode: (defaultCode: string) => set({ defaultCode }),
  resetToDefault: () => set((state) => ({ code: state.defaultCode })),
  setStdoutResult: (stdoutResult: string) =>
    set((state) => ({
      stdoutResult: [...state.stdoutResult, stdoutResult]
    })),
  resetStdoutResult: () => set({ stdoutResult: [] })
}));
