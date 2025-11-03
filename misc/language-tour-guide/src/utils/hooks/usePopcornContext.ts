import { useContext } from "react";
import {
  PopcornContext,
  type PopcornContextValue
} from "../../context/popcorn";

export const usePopcornContext = (): PopcornContextValue => {
  const context = useContext(PopcornContext);
  if (context === undefined) {
    throw new Error("usePopcornContext must be used within a PopcornProvider");
  }
  return context;
};
