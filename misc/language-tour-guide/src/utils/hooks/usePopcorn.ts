import { useCallback } from "react";
import { usePopcornContext } from "./usePopcornContext";
import type {
  AnySerializable,
  CallOptions,
  CastOptions
} from "@swmansion/popcorn";

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

  const call = useCallback(
    async (args: AnySerializable, options: CallOptions) => {
      return ensureInstance().call(args, options);
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

  return {
    call,
    cast,
    startLogCapture,
    cancelCall,
    isLoadingPopcorn,
    reinitializePopcorn
  };
};
