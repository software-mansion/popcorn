import { useCallback } from "react";
import type {
  AnySerializable,
  CallOptions,
  CastOptions
} from "../../context/popcorn";
import { usePopcornContext } from "./usePopcornContext";

export const usePopcorn = () => {
  // TODO: Popcorn should maybe have `instance.addLogListener`?
  const { instance, registerLogSink, clearCollectedOutput } =
    usePopcornContext();

  const ensureInstance = useCallback(() => {
    if (!instance) throw new Error("Popcorn instance not initialized");
    return instance;
  }, [instance]);

  const startLogCapture = () => {
    ensureInstance().registerLogSink();
    return stopLogCapture;
  };

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

  return { call, cast, startLogCapture };
};
