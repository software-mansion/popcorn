import { useState } from "react";

import ChevronDownIcon from "../../assets/chevron-down.svg?react";
import ChevronUpIcon from "../../assets/chevron-right.svg?react";
import { useExecutionHistoryStore } from "../store/executionHistory";

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

  return (
    <div className="mt-6">
      <div
        className="flex cursor-pointer items-center justify-between border-t border-orange-100 pt-3"
        onClick={toggleHistory}
      >
        <h3 className="text-brown-90 text-sm font-medium">Execution History</h3>
        <button className="text-orange-500">
          {showHistory ? (
            <ChevronUpIcon className="h-4 w-4" />
          ) : (
            <ChevronDownIcon className="h-4 w-4" />
          )}
        </button>
      </div>

      {showHistory && (
        <div className="mt-2 space-y-2">
          {history.length === 0 ? (
            <p className="text-grey-60 text-xs italic">
              No execution history yet
            </p>
          ) : (
            <>
              <div className="flex justify-end">
                <button
                  onClick={clearHistory}
                  className="text-xs text-orange-500 hover:text-orange-600"
                >
                  Clear History
                </button>
              </div>
              {history.map((entry, index) => (
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
                  <div className="rounded bg-white p-2 text-sm whitespace-pre-wrap">
                    {entry.result || "No result"}
                  </div>
                </div>
              ))}
            </>
          )}
        </div>
      )}
    </div>
  );
}
