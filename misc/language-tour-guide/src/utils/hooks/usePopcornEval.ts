import { useCallback } from "react";
import { usePopcorn } from "./usePopcorn";
import { captureCodeException } from "../sentry";

export function usePopcornEval() {
  const { call, startLogCapture } = usePopcorn();

  const evalCode = useCallback(
    async (code: string, editorId: string) => {
      const stopLogCapture = startLogCapture();

      const result = await call(["eval_elixir", editorId, code], {
        timeoutMs: 10_000
      });
      const { stderr, stdout } = stopLogCapture();

      if (!result.ok) {
        const { error, durationMs } = result;
        const errorMessage = String(error);

        captureCodeException(errorMessage, code);
        return {
          error: errorMessage,
          stderr,
          stdout,
          durationMs
        };
      }

      const { data, durationMs } = result;
      return { data, durationMs, stderr, stdout, error: null };
    },
    [call, startLogCapture]
  );

  return evalCode;
}
