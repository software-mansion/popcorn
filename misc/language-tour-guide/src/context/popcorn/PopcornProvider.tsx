import {
  useState,
  useEffect,
  type ReactNode,
  useCallback,
  useRef
} from "react";
import { PopcornContext, type Popcorn, type PopcornContextValue } from ".";
import { useCodeEditorStore } from "../../store/codeEditor";
import {
  captureAtomVmCrash,
  wrapPopcornReloadIframe
} from "../../utils/sentry";

interface PopcornProviderProps {
  children: ReactNode;
  debug?: boolean;
}

export const PopcornProvider = ({
  children,
  debug = false
}: PopcornProviderProps) => {
  const [instance, setInstance] = useState<Popcorn | null>(null);
  const setStdoutResult = useCodeEditorStore((state) => state.setStdoutResult);

  const collectedStdout = useRef<string[]>([]);
  const collectedStderr = useRef<string[]>([]);

  // TODO: use bounded buffer for collection to avoid memory issues
  const clearCollectedOutput = useCallback(() => {
    collectedStdout.current = [];
    collectedStderr.current = [];
  }, []);

  const processCollectedOutput = useCallback(() => {
    const stdout = collectedStdout.current.join("\n");
    const stderr = collectedStderr.current.join("\n");

    captureAtomVmCrash(stdout, stderr);

    clearCollectedOutput();
  }, [clearCollectedOutput]);

  const initializePopcorn = useCallback(async () => {
    try {
      const popcornInstance = await window.Popcorn.init({
        debug,
        wasmDir: import.meta.env.BASE_URL + "/wasm/",
        onStdout: (text) => {
          collectedStdout.current.push(text);

          console.log("Popcorn stdout:", text);
          setStdoutResult(text);
        },
        onStderr: (text) => {
          collectedStderr.current.push(text);

          console.error("Popcorn stderr:", text);
        }
      });

      wrapPopcornReloadIframe(popcornInstance, () => {
        processCollectedOutput();
      });

      setInstance(popcornInstance);
    } catch (error) {
      console.error("Failed to initialize Popcorn:", error);
    }
  }, [debug, setStdoutResult, processCollectedOutput]);

  const reinitializePopcorn = useCallback(() => {
    if (instance) {
      try {
        instance.deinit();
        initializePopcorn();
      } catch (e) {
        console.error("Error during Popcorn reinitialization:", e);
      }
    }
  }, [instance, initializePopcorn]);

  useEffect(() => {
    initializePopcorn();

    return () => {
      if (instance) {
        try {
          instance.deinit();
        } catch (e) {
          console.error("Error during Popcorn cleanup:", e);
        }
      }
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [debug]);

  const value: PopcornContextValue = {
    instance,
    reinitializePopcorn,
    clearCollectedOutput
  };

  return (
    <PopcornContext.Provider value={value}>{children}</PopcornContext.Provider>
  );
};
