import { useLocation } from "react-router";
import { useEffect, useRef } from "react";
import { useExecutionHistoryStore } from "../../store/executionHistory";

export function useOnNavigationChange(callback: () => void) {
  const { pathname } = useLocation();

  const clearHistory = useExecutionHistoryStore((state) => state.clearHistory);

  const callbackRef = useRef(callback);

  useEffect(() => {
    callbackRef.current = callback;
  }, [callback]);

  useEffect(() => {
    clearHistory();
    window.scrollTo({ top: 0, left: 0, behavior: "instant" });
    callbackRef.current();
  }, [pathname, clearHistory]);
}
