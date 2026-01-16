import { useCallback, useEffect, useRef, useState } from "react";
import { Button } from "../Button";
import { CodeEditor } from "./CodeEditor";
import {
  useEditorExecuting,
  useEditorChanged,
  useEditorsStore,
  useEditorExecutionState,
  useEditorQueued
} from "../../store/editors";
import { useShallow } from "zustand/react/shallow";
import { usePopcornEval } from "../../utils/hooks/usePopcornEval";
import { usePopcorn } from "../../utils/hooks/usePopcorn";
import RotatedCcw from "../../assets/rotated-ccw.svg?react";

import { useDelayedPending } from "../../utils/hooks/useDelayedPending";
import { ExecutionStateBadge } from "./ExecutionStateBadge";

type CodeDisplayProps = {
  id: string;
};

export default function CodeDisplay({ id }: CodeDisplayProps) {
  const isExecuting = useEditorExecuting(id);
  const isQueued = useEditorQueued(id);
  const isCodeChanged = useEditorChanged(id);
  const longRunning = useDelayedPending(isExecuting || isQueued, 300);
  const executionState = useEditorExecutionState(id);
  const isExecutionFailure = executionState === "failure";
  const [lastStableState, setLastStableState] = useState(executionState);

  const editorControlRef = useRef<HTMLDivElement>(null);

  const longRunningRef = useRef(longRunning);

  useEffect(() => {
    longRunningRef.current = longRunning;
  }, [longRunning]);

  useEffect(() => {
    if (isExecutionFailure) {
      window.scrollBy({
        left: 0,
        top: getOffsetTopOfCodeEditor() - window.innerHeight * 0.3,
        behavior: "smooth"
      });
    }
  }, [isExecutionFailure]);

  useEffect(() => {
    if (executionState !== "running" && executionState !== "queued") {
      setLastStableState(executionState);
    }
  }, [executionState]);

  const displayState =
    (executionState === "running" || executionState === "queued") &&
    !longRunning
      ? lastStableState
      : executionState;

  const {
    setEditorResult,
    setEditorExecutionState,
    getEditorsToRun,
    getEditor,
    resetEditorToDefault
  } = useEditorsStore(
    useShallow((state) => ({
      setEditorResult: state.setEditorResult,
      setEditorExecutionState: state.setEditorExecutionState,
      getEditorsToRun: state.getEditorsToRun,
      getEditor: state.getEditor,
      resetEditorToDefault: state.resetEditorToDefault
    }))
  );

  const evalCode = usePopcornEval();
  const { cancelCall } = usePopcorn();

  const getOffsetTopOfCodeEditor = () => {
    if (!editorControlRef.current) return 0;

    const editorControlRect = editorControlRef.current.getBoundingClientRect();

    return editorControlRect.top;
  };

  const handleRunCode = useCallback(async () => {
    const offsetTopBefore = getOffsetTopOfCodeEditor();
    if (isExecuting) return;

    const editorsToRun = getEditorsToRun(id);
    let editorFailed = false;

    for (const editorId of editorsToRun) {
      setEditorExecutionState(editorId, "queued");
    }

    for (const editorId of editorsToRun) {
      const editor = getEditor(editorId);
      if (!editor) continue;

      if (editorFailed) {
        setEditorExecutionState(editorId, "stale");
        continue;
      }

      setEditorExecutionState(editorId, "running");

      const runResult = await evalCode(editor.code, editorId);
      const { data, durationMs, stderr, stdout, error } = runResult;

      const ok = error === null;

      if (ok) {
        setEditorResult(editorId, {
          output: data,
          stdoutResult: stdout,
          stderrResult: stderr,
          durationMs
        });
        setEditorExecutionState(editorId, "success");
      } else {
        setEditorResult(editorId, {
          output: data,
          stdoutResult: stdout,
          stderrResult: stderr,
          errorMessage: error,
          durationMs
        });
        setEditorExecutionState(editorId, "failure");

        editorFailed = true;
      }
    }

    if (longRunningRef.current) {
      return;
    }

    const offsetTopAfter = getOffsetTopOfCodeEditor();
    const difference = offsetTopAfter - offsetTopBefore;

    if (difference !== 0) {
      window.scrollBy({ left: 0, top: difference, behavior: "instant" });
    }
  }, [
    isExecuting,
    id,
    evalCode,
    setEditorResult,
    setEditorExecutionState,
    getEditorsToRun,
    getEditor
  ]);

  const handleReset = useCallback(() => {
    resetEditorToDefault(id);

    setEditorExecutionState(id, "not_run");
  }, [id, resetEditorToDefault, setEditorExecutionState]);

  return (
    <>
      <div
        className="border-grey-20 flex items-center gap-2 border-b-4 bg-[#FDF6E3] px-2 py-2"
        ref={editorControlRef}
      >
        <Button
          title="Run Code"
          type="primary"
          disabled={isExecuting || isQueued}
          onClick={handleRunCode}
        />
        {isCodeChanged && (
          <Button
            type="secondary"
            title="Reset code"
            onClick={handleReset}
            Icon={RotatedCcw}
            disabled={isExecuting || isQueued}
            hideTitle
          />
        )}
        {longRunning && !isQueued && (
          <Button title="Cancel" type="secondary" onClick={cancelCall} />
        )}
        <div className="text-brown-90 ml-auto">
          <ExecutionStateBadge state={displayState} />
        </div>
      </div>

      <CodeEditor id={id} handleRunCode={handleRunCode} />
    </>
  );
}
