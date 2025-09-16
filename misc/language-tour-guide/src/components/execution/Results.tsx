import { useCallback, useEffect, useRef, useState } from "react";
import { Button } from "../Button";
import { usePopcorn } from "../../context/popcorn/actions";
import { useCodeEditorStore } from "../../store/codeEditor";

import { useShallow } from "zustand/react/shallow";
import { useExecutionHistoryStore } from "../../store/executionHistory";
import { usePending } from "../../utils/hooks/usePending";
import StdoutResults from "./StdoutResults";
import { useOnNavigationChange } from "../../utils/hooks/useOnNavigationChange";

export function Results() {
  const [durationMs, setDurationMs] = useState<number | null>(null);
  const [resultData, setResultData] = useState<string | null>(null);
  const [pending, withPending] = usePending();
  const { call } = usePopcorn();

  const { code, resetCodeToDefault, stdoutResult, resetStdoutResult } =
    useCodeEditorStore(
      useShallow((state) => ({
        setCode: state.setCode,
        code: state.code,
        resetCodeToDefault: state.resetCodeToDefault,
        stdoutResult: state.stdoutResult,
        resetStdoutResult: state.resetStdoutResult
      }))
    );

  const addHistoryEntry = useExecutionHistoryStore(
    (state) => state.addHistoryEntry
  );

  const stdoutRef = useRef<string[]>(stdoutResult);

  useEffect(() => {
    stdoutRef.current = stdoutResult;
  }, [stdoutResult]);

  const callPopcorn = useCallback(
    async (code: string) => {
      await withPending(async () => {
        let result: { data: string; durationMs: number } | null = null;

        try {
          result = await call(["eval_elixir", code], {
            timeoutMs: 10_000
          });
        } catch (error) {
          console.error("failed to initialize elixir tour:", error);
          return;
        }

        if (result === null) return;

        const { data, durationMs } = result;
        setResultData(data);
        setDurationMs(durationMs);

        addHistoryEntry({
          timestamp: new Date(),
          result: data,
          stdoutResult: stdoutRef.current,
          durationMs
        });
      });
    },
    [call, addHistoryEntry, withPending]
  );

  const handleRunCode = useCallback(() => {
    if (pending) return;

    resetStdoutResult();
    console.log("Run Code!");
    callPopcorn(code);
  }, [callPopcorn, code, resetStdoutResult, pending]);

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
    setDurationMs(null);
  }, []);

  useOnNavigationChange(resetToDefault);

  return (
    <>
      <div className="sticky top-0 flex w-full flex-wrap justify-end gap-3 border-b border-orange-100 bg-inherit py-3 pr-6">
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
