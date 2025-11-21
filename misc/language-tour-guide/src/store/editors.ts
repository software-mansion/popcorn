import { create } from "zustand";
import { immer } from "zustand/middleware/immer";
import { enableMapSet } from "immer";

enableMapSet();

export type EditorData = {
  id: string;
  code: string;
  defaultCode: string;
  output: [string, string];
  currentResult?: {
    stdoutResult?: string[];
    stderrResult?: string[];
    errorMessage?: string;
    durationMs?: number;
  };
  isExecuting: boolean;
};

type EditorsStore = {
  editors: Map<string, EditorData>;

  initEditor: (
    id: string,
    defaultCode: string,
    output: [string, string]
  ) => void;

  getEditor: (id: string) => EditorData | undefined;

  setEditorCode: (id: string, code: string) => void;

  setEditorResult: (
    id: string,
    result: {
      stdoutResult?: string[];
      stderrResult?: string[];
      errorMessage?: string;
      durationMs?: number;
    }
  ) => void;

  setEditorExecuting: (id: string, isExecuting: boolean) => void;

  resetEditorToDefault: (id: string) => void;

  isEditorCodeChanged: (id: string) => boolean;

  clearEditors: () => void;
};

export const useEditorsStore = create<EditorsStore>()(
  immer((set, get) => ({
    editors: new Map(),

    initEditor: (id: string, defaultCode: string, output: [string, string]) => {
      set((state) => {
        if (!state.editors.has(id)) {
          state.editors.set(id, {
            id,
            code: defaultCode,
            defaultCode,
            output,
            isExecuting: false
          });
        }
      });
    },

    getEditor: (id: string) => {
      return get().editors.get(id);
    },

    setEditorCode: (id: string, code: string) => {
      set((state) => {
        const editor = state.editors.get(id);
        if (editor) {
          editor.code = code;
        }
      });
    },

    setEditorResult: (id: string, result) => {
      set((state) => {
        const editor = state.editors.get(id);
        if (editor) {
          editor.currentResult = {
            ...result
          };
        }
      });
    },

    setEditorExecuting: (id: string, isExecuting: boolean) => {
      set((state) => {
        const editor = state.editors.get(id);
        if (editor) {
          editor.isExecuting = isExecuting;
        }
      });
    },

    resetEditorToDefault: (id: string) => {
      set((state) => {
        const editor = state.editors.get(id);
        if (editor) {
          editor.code = editor.defaultCode;
          editor.currentResult = undefined;
        }
      });
    },

    isEditorCodeChanged: (id: string) => {
      const editor = get().editors.get(id);
      if (!editor) return false;
      return editor.code !== editor.defaultCode;
    },

    clearEditors: () => {
      set((state) => {
        state.editors.clear();
      });
    }
  }))
);

export const useEditorCode = (id: string) =>
  useEditorsStore((state) => state.editors.get(id)?.code ?? "");

export const useEditorResult = (id: string) =>
  useEditorsStore((state) => state.editors.get(id)?.currentResult);

export const useEditorExecuting = (id: string) =>
  useEditorsStore((state) => state.editors.get(id)?.isExecuting ?? false);

export const useEditorChanged = (id: string) =>
  useEditorsStore((state) => state.isEditorCodeChanged(id));
