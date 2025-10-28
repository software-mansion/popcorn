import { useLocation } from "react-router";
import { useCodeEditorStore } from "../../store/codeEditor";
import { useEffect, useRef } from "react";
import { useExecutionHistoryStore } from "../../store/executionHistory";

export function useOnNavigationChange(callback: () => void) {
  const { pathname } = useLocation();
  const resetStdoutResult = useCodeEditorStore(
    (state) => state.resetStdoutResult
  );
  const resetStderrResult = useCodeEditorStore(
    (state) => state.resetStderrResult
  );
  const clearHistory = useExecutionHistoryStore((state) => state.clearHistory);

  const callbackRef = useRef(callback);

  useEffect(() => {
    callbackRef.current = callback;
  }, [callback]);

  useEffect(() => {
    resetStdoutResult();
    resetStderrResult();
    clearHistory();
    window.scrollTo({ top: 0, left: 0, behavior: "instant" });
    callbackRef.current();
  }, [pathname, resetStdoutResult, resetStderrResult, clearHistory]);
}
