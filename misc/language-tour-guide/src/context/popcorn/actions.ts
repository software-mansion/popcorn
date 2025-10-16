import { useCallback } from "react";
import type { AnySerializable, CallOptions, CastOptions } from ".";
import { usePopcornContext } from "../hooks";

export const usePopcorn = () => {
  const { instance, reinitializePopcorn, clearCollectedOutput } =
    usePopcornContext();

  const ensureInstance = useCallback(() => {
    if (!instance) throw new Error("Popcorn instance not initialized");
    return instance;
  }, [instance]);

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

  return {
    call,
    cast,
    reinitializePopcorn,
    clearCollectedOutput
  };
};
