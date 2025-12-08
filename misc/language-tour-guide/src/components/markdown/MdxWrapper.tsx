import { useEffect, useState } from "react";
import type { CodeSnippet } from "../../plugins/livemd/parser";
import { useEditorsStore } from "../../store/editors";
import { useOnNavigationChange } from "../../utils/hooks/useOnNavigationChange";

export type MdxWrapperProps = {
  Component: React.ComponentType;
  codeSnippets?: CodeSnippet[];
};

export function MdxWrapper({ Component, codeSnippets }: MdxWrapperProps) {
  const initEditor = useEditorsStore((state) => state.initEditor);
  const [isInitialized, setIsInitialized] = useState(false);
  useOnNavigationChange();

  useEffect(() => {
    if (!codeSnippets) {
      setIsInitialized(true);
      return;
    }

    for (const snippet of codeSnippets) {
      initEditor(snippet);
    }
    setIsInitialized(true);
  }, [codeSnippets, initEditor]);

  if (!isInitialized) {
    return null;
  }

  return <Component />;
}
