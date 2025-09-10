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
  const { pathname } = useLocation();

  const storedCode = localStorage.getItem(`code-${pathname}`);

  useEffect(() => {
    setCode(storedCode ?? code ?? "");
    setDefaultCode(code ?? "");
  }, [code, setCode, setDefaultCode, storedCode]);

  return <Component />;
}
