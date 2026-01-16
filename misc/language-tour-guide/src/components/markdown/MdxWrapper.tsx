import { useEffect, useState } from "react";
import type { CodeSnippet } from "../../plugins/livemd/parser";
import { useEditorsStore } from "../../store/editors";
import { useLocation } from "react-router";
import { usePopcorn } from "../../utils/hooks/usePopcorn";

export type MdxWrapperProps = {
  Component: React.ComponentType;
  codeSnippets?: CodeSnippet[];
};

export function MdxWrapper({ Component, codeSnippets }: MdxWrapperProps) {
  const initEditor = useEditorsStore((state) => state.initEditor);
  const clearEditors = useEditorsStore((state) => state.clearEditors);
  const [isInitialized, setIsInitialized] = useState(false);
  const { pathname } = useLocation();
  const { call } = usePopcorn();

  useEffect(() => {
    clearEditors();
  }, [pathname, clearEditors]);

  useEffect(() => {
    if (!codeSnippets) {
      setIsInitialized(true);
      return;
    }

    for (const snippet of codeSnippets) {
      initEditor(snippet);
    }

    const editorOrder = codeSnippets.map((snippet) => snippet.id);
    call(["set_editor_order", editorOrder], { timeoutMs: 5000 });

    setIsInitialized(true);
  }, [codeSnippets, initEditor, call]);

  if (!isInitialized) {
    return null;
  }

  return <Component />;
}
