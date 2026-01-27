import { createContext } from "react";
import type { Popcorn } from "@swmansion/popcorn";
export type PopcornContextValue = {
  instance: Popcorn | null;
  isLoadingPopcorn: boolean;
  reinitializePopcorn: () => void;
};

export const PopcornContext = createContext<PopcornContextValue | undefined>(
  undefined
);
