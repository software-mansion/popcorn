import { useCallback, useMemo } from "react";
import { usePopcorn } from "./usePopcorn";
import { captureCodeException } from "../sentry";
import { useLocation } from "react-router";
import { hash32 } from "../storage";

export function usePopcornEval() {
  const { call, startLogCapture } = usePopcorn();
  const { pathname } = useLocation();
  const pageId = useMemo(() => hash32(pathname), [pathname]);

  const evalCode = useCallback(
    async (code: string) => {
      const stopLogCapture = startLogCapture();

      // TODO: clean elixir context between runs?
      const { data, durationMs, error } = await call(
        ["eval_elixir", pageId, code],
        {
          timeoutMs: 10_000
        }
      );
      const { stderr, stdout } = stopLogCapture();

      if (error !== null) {
        captureCodeException(error, code);
        return { error: error, stderr, stdout, durationMs };
      }

      return { data, durationMs, stderr, stdout, error: null };
    },
    [call, startLogCapture, pageId]
  );

  return evalCode;
}
