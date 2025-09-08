import { useEffect } from "react";
import { useCodeEditorStore } from "../../store/codeEditor";

type MdxWrapperProps = {
  Component: React.ComponentType;
  code?: string;
};

export function MdxWrapper({ Component, code }: MdxWrapperProps) {
  const setCode = useCodeEditorStore((state) => state.setCode);
  const setDefaultCode = useCodeEditorStore((state) => state.setDefaultCode);

  useEffect(() => {
    setCode(code ?? "");
    setDefaultCode(code ?? "");
  }, [code, setCode, setDefaultCode]);

  return <Component />;
}
