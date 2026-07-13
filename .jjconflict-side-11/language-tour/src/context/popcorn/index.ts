import { createContext } from "react";
import type { Popcorn } from "@swmansion/popcorn";
export type PopcornStatus = "loading" | "ready" | "error";

export type PopcornContextValue = {
  instance: Popcorn | null;
  popcornStatus: PopcornStatus;
  reinitializePopcorn: () => void;
};

export const PopcornContext = createContext<PopcornContextValue | undefined>(
  undefined
);
