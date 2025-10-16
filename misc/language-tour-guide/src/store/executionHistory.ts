import { create } from "zustand";

export type HistoryEntry = {
  timestamp: Date;
  result?: string;
  durationMs?: number;
  stdoutResult?: string[];
  errorMessage?: string;
};

type ExecutionHistoryStore = {
  history: HistoryEntry[];
  addHistoryEntry: (entry: HistoryEntry) => void;
  clearHistory: () => void;
};

export const useExecutionHistoryStore = create<ExecutionHistoryStore>(
  (set) => ({
    history: [],
    addHistoryEntry: (entry: HistoryEntry) =>
      set((state) => ({
        history: [...state.history, entry]
      })),
    clearHistory: () => set({ history: [] })
  })
);
