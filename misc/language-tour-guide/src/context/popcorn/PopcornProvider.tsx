import { useState, useEffect, type ReactNode, useCallback } from "react";
import { PopcornContext, type Popcorn, type PopcornContextValue } from ".";
import { wrapPopcornReloadIframe, type LogSink } from "../../utils/sentry";

interface PopcornProviderProps {
  children: ReactNode;
  debug?: boolean;
  logSink: LogSink;
}

export const PopcornProvider = ({
  children,
  logSink,
  debug = false
}: PopcornProviderProps) => {
  const [instance, setInstance] = useState<Popcorn | null>(null);
  const [isLoadingPopcorn, setIsLoadingPopcorn] = useState<boolean>(true);

  useEffect(() => {
    let currentInstance: Popcorn | null = null;

    async function init() {
      setIsLoadingPopcorn(true);
      const { instance, error } = await initPopcorn({ debug, logSink });
      if (error !== null) {
        console.error("Error during Popcorn initialization:", error);
        return;
      }
      setIsLoadingPopcorn(false);

      currentInstance = instance;
      setInstance(instance);
    }

    init();

    return () => {
      if (currentInstance) {
        try {
          currentInstance.deinit();
        } catch (e) {
          console.error("Error during Popcorn cleanup:", e);
        }
      }
    };
  }, [debug, logSink]);

  // TODO: drop after popcorn cancel calls method is implemented (#378)
  const reinitializePopcorn = useCallback(async () => {
    if (instance) {
      try {
        instance.deinit();
      } catch (e) {
        console.error("Error during Popcorn cleanup:", e);
      }
    }

    setIsLoadingPopcorn(true);
    const { instance: newInstance, error } = await initPopcorn({
      debug,
      logSink
    });
    if (error !== null) {
      console.error("Error during Popcorn re-initialization:", error);
      return;
    }
    setIsLoadingPopcorn(false);

    setInstance(newInstance);
  }, [debug, logSink, instance]);

  const value: PopcornContextValue = {
    instance,
    isLoadingPopcorn,
    reinitializePopcorn
  };

  return (
    <PopcornContext.Provider value={value}>{children}</PopcornContext.Provider>
  );
};

type InitPopcornArgs = {
  debug: boolean;
  logSink: LogSink;
};

async function initPopcorn({
  debug,
  logSink
}: InitPopcornArgs): Promise<{ instance: Popcorn | null; error: unknown }> {
  try {
    const instance = await window.Popcorn.init({
      debug,
      wasmDir: import.meta.env.BASE_URL + "wasm/",
      onStdout: logSink.onStdout,
      onStderr: logSink.onStderr
    });

    wrapPopcornReloadIframe(instance, logSink.onCrash);
    return { instance, error: null };
  } catch (error) {
    return { instance: null, error };
  }
}
