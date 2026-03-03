import { useMemo, useState } from "react";

import ChevronDownIcon from "../../assets/chevron-down.svg?react";
import ChevronUpIcon from "../../assets/chevron-right.svg?react";
import { useExecutionHistoryStore } from "../../store/executionHistory";
import StdoutResults from "./StdoutResults";
import { CompilerError } from "./CompilerError";
import { WarningOutput } from "./WarningOutput";
import { Button } from "../Button";

function formatTimeStamp(date: Date): string {
  return date.toLocaleTimeString([], {
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit"
  });
}

function formatDuration(durationMs?: number): string {
  if (durationMs === undefined) return "N/A";
  return `${durationMs.toFixed(3)} ms`;
}

export function History() {
  const [showHistory, setShowHistory] = useState<boolean>(false);

  const history = useExecutionHistoryStore((state) => state.history);
  const clearHistory = useExecutionHistoryStore((state) => state.clearHistory);

  const toggleHistory = () => {
    setShowHistory((prev) => !prev);
  };

  const sortedHistory = useMemo(() => {
    return [...history].sort(
      (a, b) => b.timestamp.getTime() - a.timestamp.getTime()
    );
  }, [history]);

  return (
    <div className="border-t border-orange-100">
      <button
        className="mt-3 flex cursor-pointer items-center gap-5 px-6"
        onClick={toggleHistory}
      >
        <h3 className="text-brown-90 text-sm font-medium">Execution History</h3>
        {showHistory ? (
          <ChevronUpIcon className="h-4 w-4" />
        ) : (
          <ChevronDownIcon className="h-4 w-4" />
        )}
      </button>

      {showHistory && (
        <div className="mt-2 space-y-2 px-6">
          {history.length === 0 ? (
            <p className="text-grey-60 text-xs italic">
              No execution history yet
            </p>
          ) : (
            <>
              <div className="flex justify-end">
                <Button
                  type="tertiary"
                  onClick={clearHistory}
                  className="cursor-pointer text-xs font-medium text-orange-500 hover:text-orange-600"
                  title="Clear History"
                />
              </div>
              {sortedHistory.map((entry, index) => (
                <div
                  key={index}
                  className="bg-light-40 border-grey-20 rounded border p-2"
                >
                  <div className="mb-1 flex items-center justify-between">
                    <span className="text-grey-70 text-xs font-medium">
                      {formatTimeStamp(entry.timestamp)}
                    </span>
                    <span className="text-grey-60 text-xs">
                      {formatDuration(entry.durationMs)}
                    </span>
                  </div>
                  {entry.errorMessage && (
                    <CompilerError message={entry.errorMessage} />
                  )}
                  {entry.stderrResult && entry.stderrResult.length > 0 && (
                    <WarningOutput stderr={entry.stderrResult} />
                  )}
                  {entry.stdoutResult && entry.stdoutResult.length > 0 && (
                    <StdoutResults stdout={entry.stdoutResult} />
                  )}
                </div>
              ))}
            </>
          )}
        </div>
      )}
    </div>
  );
}
