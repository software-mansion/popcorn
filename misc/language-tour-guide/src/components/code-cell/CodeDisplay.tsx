import { useCallback, useState } from "react";
import { Button } from "../Button";
import { CodeEditor } from "./CodeEditor";
import {
  useEditorCode,
  useEditorExecuting,
  useEditorChanged,
  useEditorsStore
} from "../../store/editors";
import { usePopcornEval } from "../../utils/hooks/usePopcornEval";
import { usePopcorn } from "../../utils/hooks/usePopcorn";
import RotatedCcw from "../../assets/rotated-ccw.svg?react";

type CodeDisplayProps = {
  id: string;
};

export default function CodeDisplay({ id }: CodeDisplayProps) {
  const code = useEditorCode(id);
  const isExecuting = useEditorExecuting(id);
  const isCodeChanged = useEditorChanged(id);
  const setEditorExecuting = useEditorsStore(
    (state) => state.setEditorExecuting
  );
  const setEditorResult = useEditorsStore((state) => state.setEditorResult);
  const resetEditorToDefault = useEditorsStore(
    (state) => state.resetEditorToDefault
  );

  const evalCode = usePopcornEval();
  const { cancelCall } = usePopcorn();
  const [longRunning, setLongRunning] = useState(false);

  const handleRunCode = useCallback(async () => {
    if (isExecuting) return;

    setEditorExecuting(id, true);

    setLongRunning(false);

    const opts = { onLongRunning: () => setLongRunning(true) };
    const runResult = await evalCode(code, opts);
    const { data, durationMs, stderr, stdout, error } = runResult;

    const ok = error === null;

    if (ok) {
      setEditorResult(id, {
        output: data,
        stdoutResult: stdout,
        stderrResult: stderr,
        durationMs
      });
    } else {
      setEditorResult(id, {
        output: data,
        stdoutResult: stdout,
        stderrResult: stderr,
        errorMessage: error,
        durationMs
      });
    }

    setEditorExecuting(id, false);
  }, [isExecuting, id, code, evalCode, setEditorExecuting, setEditorResult]);

  const handleReset = useCallback(() => {
    resetEditorToDefault(id);
    setLongRunning(false);
  }, [id, resetEditorToDefault]);

  return (
    <>
      <div className="border-grey-20 flex items-center gap-2 border-b-4 bg-[#FDF6E3] px-2 py-2">
        <Button
          title="Run Code"
          type="primary"
          disabled={isExecuting}
          onClick={handleRunCode}
        />
        {isCodeChanged && (
          <Button
            type="secondary"
            title="Reset code"
            onClick={handleReset}
            Icon={RotatedCcw}
            disabled={isExecuting}
            hideTitle
          />
        )}
        {isExecuting && longRunning && (
          <Button title="Cancel" type="secondary" onClick={cancelCall} />
        )}
      </div>

      <CodeEditor id={id} handleRunCode={handleRunCode} />
    </>
  );
}
