import { useCallback, useEffect, useState } from "react";
import { Button } from "../Button";
import { useCodeEditorStore } from "../../store/codeEditor";

import { useShallow } from "zustand/react/shallow";
import { useExecutionHistoryStore } from "../../store/executionHistory";
import { usePending } from "../../utils/hooks/usePending";
import StdoutResults from "./StdoutResults";
import { useOnNavigationChange } from "../../utils/hooks/useOnNavigationChange";
import { CompilerError } from "./CompilerError";
import { WarningOutput } from "./WarningOutput";
import { usePopcornEval } from "../../utils/hooks/usePopcornEval";
import { usePopcorn } from "../../utils/hooks/usePopcorn";

export function Results() {
  const [durationMs, setDurationMs] = useState<number | null>(null);
  const [errorData, setErrorData] = useState<string | null>(null);
  const [pending, withPending] = usePending();
  const evalCode = usePopcornEval();
  const { cancelCall } = usePopcorn();
  const [longRunning, setLongRunning] = useState(false);
  const [stdoutResult, setStdoutResult] = useState<string[]>([]);
  const [stderrResult, setStderrResult] = useState<string[]>([]);

  const { code, resetCodeToDefault, isCodeChanged } = useCodeEditorStore(
    useShallow((state) => ({
      code: state.code,
      resetCodeToDefault: state.resetCodeToDefault,
      isCodeChanged: state.isCodeChanged
    }))
  );

  const handleCancelRunning = () => {
    cancelCall();
  };

  const addHistoryEntry = useExecutionHistoryStore(
    (state) => state.addHistoryEntry
  );

  const handleRunCode = useCallback(async () => {
    if (pending) return;

    await withPending(async () => {
      setErrorData(null);
      setDurationMs(null);
      setLongRunning(false);

      const opts = { onLongRunning: () => setLongRunning(true) };
      const result = await evalCode(code, opts);
      const { durationMs, stderr, stdout, error } = result;

      const ok = error === null;

      setStdoutResult(stdout);
      setStderrResult(stderr);

      if (ok) {
        setDurationMs(durationMs);
        addHistoryEntry({
          timestamp: new Date(),
          stdoutResult: stdout,
          stderrResult: stderr,
          durationMs: durationMs
        });
      } else {
        setErrorData(error);
        setDurationMs(durationMs);
        addHistoryEntry({
          timestamp: new Date(),
          stdoutResult: stdout,
          stderrResult: stderr,
          durationMs: durationMs,
          errorMessage: error
        });
      }
    });
  }, [addHistoryEntry, code, evalCode, pending, withPending]);

  const onKeyDown = useCallback(
    (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key === "Enter") {
        handleRunCode();
      }
    },
    [handleRunCode]
  );

  useEffect(() => {
    document.addEventListener("keydown", onKeyDown);

    return () => {
      document.removeEventListener("keydown", onKeyDown);
    };
  }, [onKeyDown]);

  const resetToDefault = useCallback(() => {
    setErrorData(null);
    setDurationMs(null);
    setStderrResult([]);
    setStdoutResult([]);
  }, []);

  useOnNavigationChange(resetToDefault);

  return (
    <>
      <div className="sticky top-0 flex w-full flex-wrap justify-end gap-3 border-b border-orange-100 bg-inherit px-6 py-3">
        <div className="mr-auto flex items-center">
          {pending ? (
            <span className="text-grey-60 text-xs"> (pending...)</span>
          ) : (
            <span className="text-grey-60 text-xs">
              {durationMs ? ` (${durationMs.toFixed(3)} ms)` : ""}
            </span>
          )}
        </div>
        {pending && longRunning && (
          <Button
            title="Cancel"
            type="secondary"
            onClick={handleCancelRunning}
          />
        )}
        {isCodeChanged() && (
          <Button
            title="Reset Code"
            type="secondary"
            onClick={resetCodeToDefault}
          />
        )}
        <Button
          title="Run Code"
          type="primary"
          disabled={pending}
          onClick={handleRunCode}
        />
      </div>
      <div className="font-inter text-brown-90 flex flex-col gap-2 px-6">
        {!pending && (
          <>
            {errorData && <CompilerError message={errorData} />}
            {stderrResult && stderrResult.length > 0 && (
              <WarningOutput stderr={stderrResult} />
            )}
            {stdoutResult && stdoutResult.length > 0 && (
              <StdoutResults stdout={stdoutResult} />
            )}
          </>
        )}
      </div>
    </>
  );
}
