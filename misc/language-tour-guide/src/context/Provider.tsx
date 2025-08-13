import { useState, useEffect, type ReactNode } from "react";
import {
  PopcornContext,
  type Popcorn,
  type PopcornContextValue
} from "./popcorn";

interface PopcornProviderProps {
  children: ReactNode;
  debug?: boolean;
}

export const PopcornProvider = ({
  children,
  debug = true
}: PopcornProviderProps) => {
  const [instance, setInstance] = useState<Popcorn | null>(null);

  useEffect(() => {
    async function initializePopcorn() {
      try {
        const popcornInstance = await window.Popcorn.init({
          debug,
          wasmDir: "/wasm/"
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
