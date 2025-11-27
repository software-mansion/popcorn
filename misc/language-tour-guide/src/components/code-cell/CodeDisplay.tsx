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
    <div className="relative">
      <div className="absolute top-2 right-2 z-10 flex gap-2">
        {isExecuting && longRunning && (
          <Button title="Cancel" type="secondary" onClick={cancelCall} />
        )}
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
        <Button
          title="Run Code"
          type="primary"
          disabled={isExecuting}
          onClick={handleRunCode}
        />
      </div>

      <CodeEditor id={id} handleRunCode={handleRunCode} />
    </div>
  );
}
