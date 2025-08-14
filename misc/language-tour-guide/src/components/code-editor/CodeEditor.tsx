import CodeMirror from "@uiw/react-codemirror";
import { useCallback } from "react";
import { elixir } from "codemirror-lang-elixir";
import { solarizedLight } from "@uiw/codemirror-theme-solarized";

import "./CodeEditor.styles.css";
import { useCodeEditorStore } from "../store/codeEditor";

export function CodeEditor() {
  const code = useCodeEditorStore((state) => state.code);
  const setCode = useCodeEditorStore((state) => state.setCode);

  const onChange = useCallback(
    (val: string) => {
      setCode(val);
    },
    [setCode]
  );

  return (
    <CodeMirror
      className="border-orange-20 scrollbar h-full min-h-[500px] overflow-x-scroll rounded-md border bg-[#FDF6E3]"
      autoFocus
      value={code}
      extensions={[elixir()]}
      onChange={onChange}
      theme={solarizedLight}
    />
  );
}
