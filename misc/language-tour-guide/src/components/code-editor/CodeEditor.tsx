import CodeMirror from "@uiw/react-codemirror";
import { elixir } from "codemirror-lang-elixir";
import { solarizedLight } from "@uiw/codemirror-theme-solarized";

import "./CodeEditor.styles.css";
import { useCodeEditorStore } from "../store/codeEditor";
import { useDefaultCode } from "../../utils/hooks/useDefaultCode";

export function CodeEditor() {
  useDefaultCode();
  const code = useCodeEditorStore((state) => state.code);
  const setCode = useCodeEditorStore((state) => state.setCode);

  return (
    <CodeMirror
      className="border-orange-20 scrollbar h-full min-h-[500px] overflow-scroll rounded-md border bg-[#FDF6E3] lg:min-h-0"
      autoFocus
      value={code}
      extensions={[elixir()]}
      onChange={setCode}
      theme={solarizedLight}
    />
  );
}
