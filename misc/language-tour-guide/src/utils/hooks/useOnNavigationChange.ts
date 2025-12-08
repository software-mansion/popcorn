import { useLocation } from "react-router";
import { useEffect, useRef } from "react";
import { useExecutionHistoryStore } from "../../store/executionHistory";
import { useEditorsStore } from "../../store/editors";

export function useOnNavigationChange(callback?: () => void) {
  const { pathname } = useLocation();

  const clearHistory = useExecutionHistoryStore((state) => state.clearHistory);
  const clearEditors = useEditorsStore((state) => state.clearEditors);

  const callbackRef = useRef(callback);

  useEffect(() => {
    callbackRef.current = callback;
  }, [callback]);

  useEffect(() => {
    clearHistory();
    clearEditors();
    window.scrollTo({ top: 0, left: 0, behavior: "instant" });
    callbackRef.current?.();
  }, [pathname, clearHistory, clearEditors]);
}
