import { useCallback, useState } from "react";
import { usePopcorn } from "./usePopcorn";
import { captureCodeException } from "../sentry";

export function usePopcornEval() {
  const { call, startOutputCapture } = usePopcorn();

  const evalCode = useCallback(
    async (code: string, { onLongRunning }: { onLongRunning: () => void }) => {
      const longRunningTimeout = setTimeout(onLongRunning, 500);
      // TODO: or other api like start/stop
      const stopOutputCapture = startOutputCapture();
      const { data, durationMs, error } = await call(["eval_elixir", code], {
        timeoutMs: 10_000
      });
      const { stderr, stdout } = stopOutputCapture();
      clearTimeout(longRunningTimeout);

      if (isDeinitializedError(error)) {
        captureCodeException(error.error.message, code);
        return { error: error.error.message };
      }

      if (isPopcornError(error)) {
        captureCodeException(error.error, code);
        return { error: error.error };
      }

      return { data, durationMs, stderr, stdout };
    },
    [call, startOutputCapture]
  );

  return evalCode;
}

type PopcornError = {
  error: string;
  durationMs?: number;
};

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
