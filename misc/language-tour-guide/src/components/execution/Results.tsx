import { useCallback, useEffect, useRef, useState } from "react";
import { Button } from "../Button";
import { usePopcorn } from "../../context/popcorn/actions";
import { useCodeEditorStore } from "../../store/codeEditor";

import { useShallow } from "zustand/react/shallow";
import { useExecutionHistoryStore } from "../../store/executionHistory";
import { usePending } from "../../utils/hooks/usePending";
import StdoutResults from "./StdoutResults";
import { useOnNavigationChange } from "../../utils/hooks/useOnNavigationChange";
import { captureCodeException } from "../../utils/sentry";
import { CompilerError } from "./CompilerError";
import { WarningOutput } from "./WarningOutput";

interface PopcornError {
  error: string;
  durationMs?: number;
}

function isPopcornError(error: unknown): error is PopcornError {
  return typeof error === "object" && error !== null && "error" in error;
}

function isDeinitializedError(error: unknown): error is { error: Error } {
  return (
    typeof error === "object" &&
    error !== null &&
    "error" in error &&
    error.error instanceof Error
  );
}

export function Results() {
  const [durationMs, setDurationMs] = useState<number | null>(null);
  const [resultData, setResultData] = useState<string | null>(null);
  const [errorData, setErrorData] = useState<string | null>(null);
  const [pending, withPending] = usePending();
  const { call, reinitializePopcorn, clearCollectedOutput } = usePopcorn();
  const [longRunning, setLongRunning] = useState(false);

  const {
    code,
    resetCodeToDefault,
    stdoutResult,
    stderrResult,
    resetStdoutResult,
    resetStderrResult
  } = useCodeEditorStore(
    useShallow((state) => ({
      setCode: state.setCode,
      code: state.code,
      resetCodeToDefault: state.resetCodeToDefault,
      stdoutResult: state.stdoutResult,
      stderrResult: state.stderrResult,
      resetStdoutResult: state.resetStdoutResult,
      resetStderrResult: state.resetStderrResult
    }))
  );

  const handleCancelRunning = () => {
    try {
      reinitializePopcorn();
      console.log("Cancel running code");
    } catch (e) {
      console.error("Error during Popcorn reinitialization:", e);
    }
  };

  const addHistoryEntry = useExecutionHistoryStore(
    (state) => state.addHistoryEntry
  );

  const stdoutRef = useRef<string[]>(stdoutResult);
  const stderrRef = useRef<string[]>(stderrResult);

  useEffect(() => {
    stdoutRef.current = stdoutResult;
  }, [stdoutResult]);

  useEffect(() => {
    stderrRef.current = stderrResult;
  }, [stderrResult]);

  const callPopcorn = useCallback(
    async (code: string) => {
      await withPending(async () => {
        let result: { data: string; durationMs: number } | null = null;

        const timeoutId = setTimeout(() => {
          setLongRunning(true);
        }, 500);

        setErrorData(null);
        setResultData(null);
        setDurationMs(null);

        try {
          result = await call(["eval_elixir", code], {
            timeoutMs: 10_000
          });
        } catch (error: unknown) {
          console.error("Error executing Elixir code:", error);

          if (isDeinitializedError(error)) {
            setErrorData(error.error.message);
            captureCodeException(error.error.message, code);
            return;
          }

          if (isPopcornError(error)) {
            captureCodeException(error.error, code);

            setErrorData(error.error);
            setDurationMs(error.durationMs || null);

            addHistoryEntry({
              timestamp: new Date(),
              stdoutResult: stdoutRef.current,
              stderrResult: stderrRef.current,
              durationMs: error.durationMs,
              errorMessage: error.error
            });
          } else {
            setErrorData("Unknown error");
          }
        }

        clearCollectedOutput();

        if (result === null) return;

        clearTimeout(timeoutId);

        const { data, durationMs } = result;

        // TODO: remove escape sequences from stderr
        setResultData(data);
        setDurationMs(durationMs);

        addHistoryEntry({
          timestamp: new Date(),
          result: data,
          stdoutResult: stdoutRef.current,
          stderrResult: stderrRef.current,
          durationMs
        });
      });
    },
    [call, addHistoryEntry, withPending, clearCollectedOutput]
  );

  const handleRunCode = useCallback(() => {
    if (pending) return;

    resetStdoutResult();
    resetStderrResult();
    console.log("Run Code!");
    callPopcorn(code);
  }, [callPopcorn, code, resetStdoutResult, resetStderrResult, pending]);

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
    setResultData(null);
    setErrorData(null);
    setDurationMs(null);
  }, []);

  useOnNavigationChange(resetToDefault);

  return (
    <>
      <div className="sticky top-0 flex w-full flex-wrap justify-end gap-3 border-b border-orange-100 bg-inherit py-3 pr-6">
        {pending && longRunning && (
          <Button
            title="Cancel"
            type="secondary"
            onClick={handleCancelRunning}
          />
        )}
        <Button
          title="Reset Code"
          type="secondary"
          onClick={resetCodeToDefault}
        />
        <Button
          title="Run Code"
          type="primary"
          disabled={pending}
          onClick={handleRunCode}
        />
      </div>
      <div className="font-inter text-brown-90 flex flex-col gap-2 px-6">
        {pending ? (
          <span className="text-grey-60 text-xs"> (pending...)</span>
        ) : (
          <>
            <span className="text-grey-60 text-xs">
              {durationMs ? ` (${durationMs.toFixed(3)} ms)` : ""}
            </span>
            {errorData && <CompilerError message={errorData} />}
            {stderrResult && stderrResult.length > 0 && (
              <WarningOutput stderr={stderrResult} />
            )}
            {stdoutResult && stdoutResult.length > 0 && (
              <StdoutResults stdout={stdoutResult} />
            )}
            <span>{resultData}</span>
          </>
        )}
      </div>
    </>
  );
}
