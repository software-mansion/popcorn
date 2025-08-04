import type { AnySerializable, CallOptions, CastOptions } from ".";
import { usePopcornContext } from "../hooks";

export const usePopcornActions = () => {
  const { instance } = usePopcornContext();

  const call = async (args: AnySerializable, options: CallOptions) => {
    if (!instance) {
      throw new Error("Popcorn not initialized");
    }

    return instance.call(args, options);
  };

  const cast = (args: AnySerializable, options: CastOptions) => {
    if (!instance) {
      throw new Error("Popcorn not initialized");
    }
    return instance.cast(args, options);
  };

  return {
    call,
    cast,
  };
};
