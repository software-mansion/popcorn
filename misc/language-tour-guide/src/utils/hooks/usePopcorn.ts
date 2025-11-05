import { useCallback } from "react";
import type {
  AnySerializable,
  CallOptions,
  CastOptions,
  PopcornError
} from "../../context/popcorn";
import { usePopcornContext } from "./usePopcornContext";

export const usePopcorn = () => {
  const { instance, reinitializePopcorn, isLoadingPopcorn } =
    usePopcornContext();

  const ensureInstance = useCallback(() => {
    if (!instance) throw new Error("Popcorn instance not initialized");
    return instance;
  }, [instance]);

  const startLogCapture = useCallback(() => {
    const stdout: string[] = [];
    const stderr: string[] = [];

    const stdoutListener = (message: string) => {
      stdout.push(message);
    };

    const stderrListener = (message: string) => {
      stderr.push(message);
    };

    ensureInstance().registerLogListener(stdoutListener, "stdout");
    ensureInstance().registerLogListener(stderrListener, "stderr");

    const stopLogCapture = () => {
      ensureInstance().unregisterLogListener(stdoutListener, "stdout");
      ensureInstance().unregisterLogListener(stderrListener, "stderr");
      return {
        stdout,
        stderr
      };
    };

    return stopLogCapture;
  }, [ensureInstance]);

  // TODO: popcorn should never throw, an exception on call
  // drop it after #378 is implemented
  const call = useCallback(
    async (args: AnySerializable, options: CallOptions) => {
      try {
        console.log("Popcorn call args:", args, "options:", options);

        const result = await ensureInstance().call(args, options);
        return { ...result, error: null };
      } catch (error) {
        if (isDeinitializedError(error)) {
          return { error: error.error.message, durationMs: 0, data: null };
        }

        if (isPopcornError(error)) {
          return { error: error.error, durationMs: 0, data: null };
        }

        return { error: "Popcorn call error", durationMs: 0, data: null };
      }
    },
    [ensureInstance]
  );

  const cast = useCallback(
    (args: AnySerializable, options: CastOptions) => {
      return ensureInstance().cast(args, options);
    },
    [ensureInstance]
  );

  // TODO: replace with popcorn cancelCall method after #378 is implemented
  const cancelCall = useCallback(() => {
    reinitializePopcorn();
  }, [reinitializePopcorn]);

  return { call, cast, startLogCapture, cancelCall, isLoadingPopcorn };
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
