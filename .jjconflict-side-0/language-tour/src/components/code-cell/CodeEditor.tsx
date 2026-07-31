import { memo, useMemo } from "react";
import CodeMirror from "@uiw/react-codemirror";
import { elixir } from "codemirror-lang-elixir";
import { solarizedLightInit } from "@uiw/codemirror-theme-solarized";
import { keymap } from "@codemirror/view";
import { Prec } from "@codemirror/state";

import "./CodeEditor.styles.css";
import { useEditorCode, useEditorsStore } from "../../store/editors";

type CodeEditorProps = {
  id: string;
  handleRunCode: () => void;
};

export const CodeEditor = memo(function CodeEditor({
  id,
  handleRunCode
}: CodeEditorProps) {
  const code = useEditorCode(id);
  const setCode = useEditorsStore((state) => state.setEditorCode);

  const keymapExtension = useMemo(
    () =>
      Prec.highest(
        keymap.of([
          {
            key: "Cmd-Enter",
            run: () => {
              handleRunCode();
              return true;
            }
          },
          {
            key: "Ctrl-Enter",
            run: () => {
              handleRunCode();
              return true;
            }
          }
        ])
      ),
    [handleRunCode]
  );

  return (
    <CodeMirror
      className="h-full min-h-16 bg-[#FDF6E3]"
      autoFocus
      value={code}
      extensions={[elixir(), keymapExtension]}
      onChange={(value) => setCode(id, value)}
      theme={solarizedLightInit({ settings: { fontSize: "14px" } })}
    />
  );
});
