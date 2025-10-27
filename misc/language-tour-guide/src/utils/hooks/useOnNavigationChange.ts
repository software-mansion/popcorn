import { useLocation } from "react-router";
import { useCodeEditorStore } from "../../store/codeEditor";
import { useEffect } from "react";
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

  useEffect(() => {
    resetStdoutResult();
    resetStderrResult();
    clearHistory();
    window.scrollTo({ top: 0, left: 0, behavior: "instant" });
    callback();
  }, [pathname, callback, resetStdoutResult, resetStderrResult, clearHistory]);
}
