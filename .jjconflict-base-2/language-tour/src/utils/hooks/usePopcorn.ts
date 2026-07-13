import { useCallback } from "react";
import { usePopcornContext } from "./usePopcornContext";
import { startLogCapture } from "../sentry";

export const usePopcorn = () => {
  const { instance, reinitializePopcorn, popcornStatus } = usePopcornContext();

  const ensureInstance = useCallback(() => {
    if (!instance) throw new Error("Popcorn instance not initialized");
    return instance;
  }, [instance]);

  const call = useCallback(
    async (args: unknown, options: { process?: string; timeoutMs?: number }) => {
      const start = performance.now();
      const result = await ensureInstance().genserver.call(
        options.process ?? "main",
        args,
        { timeoutMs: options.timeoutMs }
      );
      const durationMs = performance.now() - start;
      if (!result.ok) return { ...result, durationMs };

      const reply = result.data as { data?: unknown; error?: string };
      return reply.error === undefined
        ? { ok: true as const, data: reply.data, durationMs }
        : { ok: false as const, error: reply.error, durationMs };
    },
    [ensureInstance]
  );

  const cast = useCallback(
    (args: unknown, options: { process?: string }) => {
      return ensureInstance().genserver.cast(options.process ?? "main", args);
    },
    [ensureInstance]
  );

  // TODO: replace with popcorn cancelCall method after #378 is implemented
  const cancelCall = useCallback(() => {
    reinitializePopcorn();
  }, [reinitializePopcorn]);

  return {
    call,
    cast,
    startLogCapture,
    cancelCall,
    popcornStatus,
    reinitializePopcorn
  };
};
