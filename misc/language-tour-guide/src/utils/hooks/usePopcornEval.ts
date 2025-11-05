import { useCallback } from "react";
import { usePopcorn } from "./usePopcorn";
import { captureCodeException } from "../sentry";

export function usePopcornEval() {
  const { call, startLogCapture } = usePopcorn();

  const evalCode = useCallback(
    async (code: string, { onLongRunning }: { onLongRunning: () => void }) => {
      const longRunningTimeout = setTimeout(onLongRunning, 500);
      const stopLogCapture = startLogCapture();

      const { data, durationMs, error } = await call(["eval_elixir", code], {
        timeoutMs: 10_000
      });
      const { stderr, stdout } = stopLogCapture();
      clearTimeout(longRunningTimeout);

      if (error !== null) {
        captureCodeException(error, code);
        return { error: error, stderr, stdout, durationMs };
      }

      return { data, durationMs, stderr, stdout, error: null };
    },
    [call, startLogCapture]
  );

  return evalCode;
}
