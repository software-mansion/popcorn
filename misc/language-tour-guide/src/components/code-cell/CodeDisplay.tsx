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
import { useDelayedPending } from "../../utils/hooks/useDelayedPending";

type CodeDisplayProps = {
  id: string;
};

export default function CodeDisplay({ id }: CodeDisplayProps) {
  const code = useEditorCode(id);
  const isExecuting = useEditorExecuting(id);
  const isCodeChanged = useEditorChanged(id);
  const longRunning = useDelayedPending(isExecuting, 500);

  const setEditorExecuting = useEditorsStore(
    (state) => state.setEditorExecuting
  );
  const setEditorResult = useEditorsStore((state) => state.setEditorResult);
  const resetEditorToDefault = useEditorsStore(
    (state) => state.resetEditorToDefault
  );

  const evalCode = usePopcornEval();
  const { cancelCall } = usePopcorn();

  const handleRunCode = useCallback(async () => {
    if (isExecuting) return;

    setEditorExecuting(id, true);

    const runResult = await evalCode(code);
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
    setEditorExecuting(id, false);
  }, [id, resetEditorToDefault, setEditorExecuting]);

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
        {longRunning && (
          <Button title="Cancel" type="secondary" onClick={cancelCall} />
        )}
      </div>

      <CodeEditor id={id} handleRunCode={handleRunCode} />
    </>
  );
}
