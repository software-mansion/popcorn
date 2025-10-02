import { useEffect } from "react";
import { useCodeEditorStore } from "../../store/codeEditor";
import { useLocation } from "react-router";

type MdxWrapperProps = {
  Component: React.ComponentType;
  code?: string;
};

export function MdxWrapper({ Component, code }: MdxWrapperProps) {
  const setCode = useCodeEditorStore((state) => state.setCode);
  const setDefaultCode = useCodeEditorStore((state) => state.setDefaultCode);
  const getCodeFromStorage = useCodeEditorStore(
    (state) => state.getCodeFromStorage
  );
  const setPathHash = useCodeEditorStore((state) => state.setPathHash);
  const { pathname } = useLocation();

  useEffect(() => {
    setPathHash(pathname);
    const storedCode = getCodeFromStorage();

    setCode(storedCode ?? code ?? "");
    setDefaultCode(code ?? "");
  }, [
    code,
    setCode,
    setDefaultCode,
    getCodeFromStorage,
    setPathHash,
    pathname
  ]);

  return <Component />;
}
