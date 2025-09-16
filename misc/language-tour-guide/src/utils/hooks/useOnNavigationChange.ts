import { useLocation } from "react-router";
import { useCodeEditorStore } from "../../store/codeEditor";
import { useEffect } from "react";

export function useOnNavigationChange(callback: () => void) {
  const { pathname } = useLocation();
  const setPathHash = useCodeEditorStore((state) => state.setPathHash);
  const resetStdoutResult = useCodeEditorStore(
    (state) => state.resetStdoutResult
  );

  useEffect(() => {
    setPathHash(pathname);
    resetStdoutResult();
    callback();
  }, [pathname, callback, setPathHash, resetStdoutResult]);
}
