import { useCallback, useEffect, useState } from "react";
import { Button } from "../Button";
import { usePopcorn } from "../../context/popcorn/actions";
import { useCodeEditorStore } from "../store/codeEditor";

import { History } from "./History";
import { useExecutionHistoryStore } from "../store/executionHistory";

export function Results() {
  const [durationMs, setDurationMs] = useState<number | null>(null);
  const [resultData, setResultData] = useState<string | null>(null);
  const [pending, setPending] = useState<boolean>(false);

  const code = useCodeEditorStore((state) => state.code);
  const resetToDefault = useCodeEditorStore((state) => state.resetToDefault);
  const stdoutResult = useCodeEditorStore((state) => state.stdoutResult);
  const resetStdoutResult = useCodeEditorStore(
    (state) => state.resetStdoutResult
  );
  const addHistoryEntry = useExecutionHistoryStore(
    (state) => state.addHistoryEntry
  );

  const { call } = usePopcorn();

  const callPopcorn = useCallback(
    async (code: string) => {
      try {
        setPending(true);
        const { data, durationMs } = await call(["eval_elixir", code], {
          timeoutMs: 10_000
        });

        setPending(false);
        setResultData(data);
        setDurationMs(durationMs);

        // Add to history
        addHistoryEntry({
          timestamp: new Date(),
          code,
          result: data,
          durationMs
        });
      } catch (error) {
        setPending(false);
        console.error("failed to initialize elixir tour:", error);
      }
    },
    [call, addHistoryEntry]
  );

  const handleRunCode = useCallback(() => {
    resetStdoutResult();
    console.log("Run Code!");
    callPopcorn(code);
  }, [callPopcorn, code, resetStdoutResult]);

  const onKeyDown = useCallback(
    (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key === "Enter") {
        handleRunCode();
      }
    },
    [handleRunCode]
  );

  const handleResetCode = useCallback(() => {
    resetToDefault();
  }, [resetToDefault]);

  useEffect(() => {
    document.addEventListener("keydown", onKeyDown);

    return () => {
      document.removeEventListener("keydown", onKeyDown);
    };
  }, [onKeyDown]);

  return (
    <section className="bg-light-30 border-grey-20 scrollbar min-h-60 overflow-y-scroll rounded-md border pb-4">
      <div className="flex w-full flex-wrap justify-end gap-3 border-b border-orange-100 py-3 pr-6">
        <Button title="Reset Code" type="secondary" onClick={handleResetCode} />
        {/* <Button title="Format Code" type="secondary" /> */}
        <Button title="Run Code" type="primary" onClick={handleRunCode} />
      </div>
      <div className="font-inter text-brown-90 mt-4 flex flex-col gap-2 overflow-hidden px-6">
        {pending ? (
          <span className="text-grey-60 text-xs"> (pending...)</span>
        ) : (
          <>
            <span className="text-grey-60 text-xs">
              {durationMs ? ` (${durationMs.toFixed(3)} ms)` : ""}
            </span>
            {stdoutResult.map((line, index) => (
              <span
                key={`stdout-${index}-${line}`}
                className="text-brown-90/70 text-xs font-medium"
              >
                {line}
              </span>
            ))}
            <span>{resultData}</span>
          </>
        )}

        <History />
      </div>
    </section>
  );
}
