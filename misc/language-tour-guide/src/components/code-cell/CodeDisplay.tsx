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
import { useScrollLock } from "../../utils/hooks/useScrollLock";
import { useExecutionToast } from "../../utils/hooks/useExecutionToast";
import { EditorToast } from "./EditorToast";

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
  const isExecutionSuccess = executionState === "success";
  const [lastStableState, setLastStableState] = useState(executionState);

  const editorControlRef = useRef<HTMLDivElement>(null);

  const { lockScroll, unlockScroll } = useScrollLock(editorControlRef);

  const { showCompleted, scrollToElement, dismissToast, startTracking } =
    useExecutionToast({
      isExecutionSuccess,
      elementRef: editorControlRef
    });

  useEffect(() => {
    if (isExecutionFailure && editorControlRef.current) {
      editorControlRef.current.scrollIntoView({
        behavior: "smooth",
        block: "center"
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

  const handleRunCode = useCallback(async () => {
    if (isExecuting) return;

    startTracking();
    lockScroll();

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

    unlockScroll();
  }, [
    isExecuting,
    id,
    evalCode,
    setEditorResult,
    setEditorExecutionState,
    getEditorsToRun,
    getEditor,
    lockScroll,
    unlockScroll,
    startTracking
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
      <EditorToast
        isVisible={showCompleted}
        onClick={scrollToElement}
        onClose={dismissToast}
      />
    </>
  );
}
