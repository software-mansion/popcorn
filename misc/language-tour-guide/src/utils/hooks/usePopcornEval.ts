import { useCallback } from "react";
import { usePopcorn } from "./usePopcorn";
import { captureCodeException } from "../sentry";
import { useEditorsStore } from "../../store/editors";

export function usePopcornEval() {
  const { call, startLogCapture } = usePopcorn();
  const editorOrder = useEditorsStore((state) => state.editorOrder);

  const evalCode = useCallback(
    async (code: string, editorId: string) => {
      const stopLogCapture = startLogCapture();

      const result = await call(["eval_elixir", editorId, code, editorOrder], {
        timeoutMs: 30_000
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
    [call, startLogCapture, editorOrder]
  );

  return evalCode;
}
