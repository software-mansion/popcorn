import CodeMirror from "@uiw/react-codemirror";
import { elixir } from "codemirror-lang-elixir";
import { solarizedLight } from "@uiw/codemirror-theme-solarized";
import { keymap } from "@codemirror/view";
import { Prec } from "@codemirror/state";

import "./CodeEditor.styles.css";
import { useCodeEditorStore } from "../../store/codeEditor";
import { useMemo } from "react";

export function CodeEditor() {
  const code = useCodeEditorStore((state) => state.code);
  const setCode = useCodeEditorStore((state) => state.setCode);

  const keymapExtension = useMemo(() => {
    return Prec.highest(
      keymap.of([
        {
          key: "Cmd-Enter",
          run: () => {
            return true;
          }
        },
        {
          key: "Ctrl-Enter",
          run: () => {
            return true;
          }
        }
      ])
    );
  }, []);

  return (
    <CodeMirror
      className="border-orange-20 scrollbar h-full min-h-[500px] overflow-scroll rounded-md border bg-[#FDF6E3] lg:min-h-0"
      autoFocus
      value={code}
      extensions={[elixir(), keymapExtension]}
      onChange={setCode}
      theme={solarizedLight}
    />
  );
}
