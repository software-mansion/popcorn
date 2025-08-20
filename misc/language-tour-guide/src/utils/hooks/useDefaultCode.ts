import { useEffect } from "react";
import { useCodeEditorStore } from "../../components/store/codeEditor";
import { useLocation } from "react-router";
import { getNode } from "../content/tree-builder";

export function useDefaultCode() {
  const { pathname } = useLocation();

  const setDefaultCode = useCodeEditorStore((state) => state.setDefaultCode);
  const setCode = useCodeEditorStore((state) => state.setCode);

  useEffect(() => {
    const getNodeAsync = async () => {
      const path = pathname.slice(1).split("/");

      const result = await getNode(path);

      const defaultCode = result?.frontmatter.defaultCode ?? "";

      setDefaultCode(defaultCode);
      setCode(defaultCode);
    };

    getNodeAsync();
  }, [pathname, setDefaultCode, setCode]);

  return null;
}
