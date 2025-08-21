import { useState, useEffect, type ReactNode } from "react";
import { PopcornContext, type Popcorn, type PopcornContextValue } from ".";
import { useCodeEditorStore } from "../../components/store/codeEditor";

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

  useEffect(() => {
    async function initializePopcorn() {
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
    }

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
    instance
  };

  return (
    <PopcornContext.Provider value={value}>{children}</PopcornContext.Provider>
  );
};
