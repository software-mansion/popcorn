import { useEffect } from "react";
import { useCodeEditorStore } from "../../store/codeEditor";

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

  useEffect(() => {
    const storedCode = getCodeFromStorage();

    setCode(storedCode ?? code ?? "");
    setDefaultCode(code ?? "");
  }, [code, setCode, setDefaultCode, getCodeFromStorage]);

  return <Component />;
}
