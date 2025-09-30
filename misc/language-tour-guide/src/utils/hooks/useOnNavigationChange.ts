import { useLocation } from "react-router";
import { useCodeEditorStore } from "../../store/codeEditor";
import { useEffect } from "react";

export function useOnNavigationChange(callback: () => void) {
  const { pathname } = useLocation();
  const resetStdoutResult = useCodeEditorStore(
    (state) => state.resetStdoutResult
  );

  useEffect(() => {
    resetStdoutResult();
    callback();
  }, [pathname, callback, resetStdoutResult]);
}
