import {
  useState,
  useEffect,
  type ReactNode,
  useCallback,
  useRef
} from "react";
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

  useEffect(() => {
    async function init() {
      const { instance, error } = await initPopcorn({ debug, ...logSink });
      if (error !== null) {
        console.error("Error during Popcorn reinitialization:", error);
        return;
      }

      setInstance(instance!);
    }
    init();

    return () => {
      instance?.deinit();
    };
  }, [debug, instance, logSink]);

  const value: PopcornContextValue = {
    instance,
    reinitializePopcorn: TODO_toRemove,
    clearCollectedOutput: TODO_toRemove
  };

  return (
    <PopcornContext.Provider value={value}>{children}</PopcornContext.Provider>
  );
};

function TODO_toRemove() {}

type InitPopcornArgs = {
  debug: boolean;
} & LogSink;
async function initPopcorn({
  debug,
  onStdout,
  onStderr,
  onCrash
}: InitPopcornArgs) {
  try {
    const instance = await window.Popcorn.init({
      debug,
      wasmDir: import.meta.env.BASE_URL + "wasm/",
      onStdout,
      onStderr
    });

    wrapPopcornReloadIframe(instance, onCrash);
    return { instance, error: null };
  } catch (error) {
    return { instance: null, error };
  }
}
