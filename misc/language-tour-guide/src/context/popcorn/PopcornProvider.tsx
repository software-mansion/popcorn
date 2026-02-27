import {
  useState,
  useEffect,
  useRef,
  type ReactNode,
  useCallback
} from "react";
import {
  PopcornContext,
  type PopcornContextValue,
  type PopcornStatus
} from ".";
import { Popcorn } from "@swmansion/popcorn";
import { type LogSink } from "../../utils/sentry";

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
  const [popcornStatus, setPopcornStatus] = useState<PopcornStatus>("loading");
  const instanceRef = useRef<Popcorn | null>(null);
  const isReinitializingRef = useRef(false);

  useEffect(() => {
    async function init() {
      isReinitializingRef.current = true;
      setPopcornStatus("loading");
      const { instance, error } = await initPopcorn({ debug, logSink });

      if (error !== null) {
        console.error("Error during Popcorn initialization:", error);
        isReinitializingRef.current = false;
        setPopcornStatus("error");
        return;
      }

      instanceRef.current = instance;
      setInstance(instance);
      setPopcornStatus("ready");
      isReinitializingRef.current = false;
    }

    init();

    return () => {
      isReinitializingRef.current = false;
      if (instanceRef.current) {
        try {
          instanceRef.current.deinit();
        } catch (e) {
          console.error("Error during Popcorn cleanup:", e);
        }
        instanceRef.current = null;
      }
    };
  }, [debug, logSink]);

  // TODO: drop after popcorn cancel calls method is implemented (#378)
  const reinitializePopcorn = useCallback(async () => {
    if (isReinitializingRef.current) {
      return;
    }

    isReinitializingRef.current = true;

    if (instanceRef.current) {
      try {
        instanceRef.current.deinit();
      } catch (e) {
        console.error("Error during Popcorn cleanup:", e);
      }
      instanceRef.current = null;
    }

    setPopcornStatus("loading");
    const { instance: newInstance, error } = await initPopcorn({
      debug,
      logSink
    });

    if (error !== null) {
      console.error("Error during Popcorn re-initialization:", error);
      isReinitializingRef.current = false;
      setPopcornStatus("error");
      return;
    }

    instanceRef.current = newInstance;
    setInstance(newInstance);
    setPopcornStatus("ready");
    isReinitializingRef.current = false;
  }, [debug, logSink]);

  const value: PopcornContextValue = {
    instance,
    popcornStatus,
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
    const instance = await Popcorn.init({
      debug,
      onStdout: logSink.onStdout,
      onStderr: logSink.onStderr,
      // TODO(jgonet): prepare closed error set for reloads
      onReload: logSink.onCrash as (reason: string) => void
    });

    return { instance, error: null };
  } catch (error) {
    return { instance: null, error };
  }
}
