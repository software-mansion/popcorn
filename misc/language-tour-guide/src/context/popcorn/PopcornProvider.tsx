import { useState, useEffect, type ReactNode, useCallback } from "react";
import { PopcornContext, type Popcorn, type PopcornContextValue } from ".";
import { useCodeEditorStore } from "../../store/codeEditor";

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

  const initializePopcorn = useCallback(async () => {
    try {
      const popcornInstance = await window.Popcorn.init({
        debug,
        wasmDir: import.meta.env.BASE_URL + "/wasm/",
        onStdout: (text) => {
          console.log("Popcorn stdout:", text);
          setStdoutResult(text);
        }
      });

      setInstance(popcornInstance);
    } catch (error) {
      console.error("Failed to initialize Popcorn:", error);
    }
  }, [debug, setStdoutResult]);

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
    reinitializePopcorn
  };

  return (
    <PopcornContext.Provider value={value}>{children}</PopcornContext.Provider>
  );
};
