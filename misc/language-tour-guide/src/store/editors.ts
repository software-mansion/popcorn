import { create } from "zustand";
import { immer } from "zustand/middleware/immer";
import { enableMapSet } from "immer";
import type { CodeSnippet } from "../plugins/livemd/parser";
import {
  getSavedEditorState,
  saveEditorState,
  removeSavedEditorState
} from "../utils/storage";

enableMapSet();

const saveDebounceTimers = new Map<string, ReturnType<typeof setTimeout>>();
const SAVE_DEBOUNCE_MS = 500;

function debouncedSaveEditorState(
  id: string,
  code: string,
  defaultCode: string
) {
  const existingTimer = saveDebounceTimers.get(id);
  if (existingTimer) {
    clearTimeout(existingTimer);
  }

  const timer = setTimeout(() => {
    if (code !== defaultCode) {
      saveEditorState(id, { code, state: "not_run" });
    } else {
      removeSavedEditorState(id);
    }
    saveDebounceTimers.delete(id);
  }, SAVE_DEBOUNCE_MS);

  saveDebounceTimers.set(id, timer);
}

export type ExecutionState =
  | "not_run"
  | "success"
  | "failure"
  | "stale"
  | "running"
  | "queued";

export type EditorData = {
  id: string;
  code: string;
  defaultCode: string;
  testReplaceCode?: string;
  currentResult?: {
    stdoutResult?: string[];
    stderrResult?: string[];
    errorMessage?: string;
    durationMs?: number;
    output?: any;
  };
  executionState: ExecutionState;
};

type EditorsStore = {
  editors: Map<string, EditorData>;
  editorOrder: string[];

  initEditor: (codeSnippet: CodeSnippet) => void;

  getEditor: (id: string) => EditorData | undefined;

  setEditorCode: (id: string, code: string) => void;

  setEditorResult: (
    id: string,
    result: {
      stdoutResult?: string[];
      stderrResult?: string[];
      errorMessage?: string;
      durationMs?: number;
      output?: any;
    }
  ) => void;

  setEditorExecutionState: (id: string, state: ExecutionState) => void;

  markFollowingEditorsAsStale: (id: string) => void;

  getEditorsToRun: (id: string) => string[];

  resetEditorToDefault: (id: string) => void;

  isEditorCodeChanged: (id: string) => boolean;

  clearEditors: () => void;
};

export const useEditorsStore = create<EditorsStore>()(
  immer((set, get) => ({
    editors: new Map(),
    editorOrder: [],

    initEditor: (codeSnippet: CodeSnippet) => {
      const {
        id,
        initCode: defaultCode,
        stdout,
        output,
        testReplaceCode
      } = codeSnippet;

      set((state) => {
        if (!state.editors.has(id)) {
          const savedState = getSavedEditorState(id);
          const code = savedState?.code ?? defaultCode;
          const editorState = savedState?.state;

          state.editors.set(id, {
            id,
            code,
            defaultCode,
            ...(testReplaceCode && { testReplaceCode }),
            executionState: editorState ?? (output ? "success" : "not_run"),
            currentResult:
              editorState !== "success" && !output
                ? undefined
                : {
                    stdoutResult: stdout,
                    output
                  }
          });
          state.editorOrder.push(id);
        }
      });
    },

    getEditor: (id: string) => {
      return get().editors.get(id);
    },

    setEditorCode: (id: string, code: string) => {
      const editor = get().editors.get(id);
      if (!editor) return;

      set((state) => {
        const editor = state.editors.get(id);
        if (editor) {
          editor.code = code;

          if (
            code !== editor.defaultCode &&
            editor.executionState === "success"
          ) {
            editor.executionState = "stale";
          }
        }
      });

      debouncedSaveEditorState(id, code, editor.defaultCode);
      get().markFollowingEditorsAsStale(id);
    },

    setEditorResult: (id: string, result) => {
      set((state) => {
        const editor = state.editors.get(id);
        if (editor) {
          editor.currentResult = structuredClone(result);
        }
      });
    },

    setEditorExecutionState: (id: string, executionState: ExecutionState) => {
      set((state) => {
        const editor = state.editors.get(id);
        if (editor) {
          editor.executionState = executionState;
        }
      });
    },

    markFollowingEditorsAsStale: (id: string) => {
      const state = get();
      const currentIndex = state.editorOrder.indexOf(id);
      if (currentIndex === -1) return;

      set((state) => {
        for (let i = currentIndex + 1; i < state.editorOrder.length; i++) {
          const editorId = state.editorOrder[i];
          const editor = state.editors.get(editorId);

          if (editor && editor.executionState !== "not_run") {
            editor.executionState = "stale";
            saveEditorState(editorId, { code: null, state: "not_run" });
          }
        }
      });
    },

    getEditorsToRun: (id: string) => {
      const state = get();
      const currentIndex = state.editorOrder.indexOf(id);

      // run all editors from the last non-successful one up to the current one
      let startIndex = currentIndex;
      for (let i = 0; i < currentIndex; i++) {
        const editorId = state.editorOrder[i];
        const editor = state.editors.get(editorId);

        if (editor && editor.executionState !== "success") {
          startIndex = i;
          break;
        }
      }

      return state.editorOrder.slice(startIndex, currentIndex + 1);
    },

    resetEditorToDefault: (id: string) => {
      set((state) => {
        const editor = state.editors.get(id);
        if (editor) {
          editor.code = editor.defaultCode;
          editor.currentResult = undefined;
          editor.executionState = "not_run";
        }
      });
      removeSavedEditorState(id);
      get().markFollowingEditorsAsStale(id);
    },

    isEditorCodeChanged: (id: string) => {
      const editor = get().editors.get(id);
      if (!editor) return false;
      return editor.code !== editor.defaultCode;
    },

    clearEditors: () => {
      set((state) => {
        state.editors.clear();
        state.editorOrder = [];
      });
    }
  }))
);

export const useEditorCode = (id: string) =>
  useEditorsStore((state) => state.editors.get(id)?.code ?? "");

export const useEditorResult = (id: string) =>
  useEditorsStore((state) => state.editors.get(id)?.currentResult);

export const useEditorExecuting = (id: string) =>
  useEditorsStore(
    (state) => state.editors.get(id)?.executionState === "running"
  );

export const useEditorQueued = (id: string) =>
  useEditorsStore(
    (state) => state.editors.get(id)?.executionState === "queued"
  );

export const useEditorChanged = (id: string) =>
  useEditorsStore((state) => state.isEditorCodeChanged(id));

export const useEditorExecutionState = (id: string) =>
  useEditorsStore(
    (state) => state.editors.get(id)?.executionState ?? "not_run"
  );
