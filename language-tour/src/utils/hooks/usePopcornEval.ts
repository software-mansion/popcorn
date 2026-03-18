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

      const timeout = import.meta.env.VITE_PLAYWRIGHT_TEST ? 160_000 : 45_000;

      const result = await call(["eval_elixir", editorId, code, editorOrder], {
        timeoutMs: timeout
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
