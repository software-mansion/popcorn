import type { ReactNode } from "react";
import { usePopcorn } from "../../utils/hooks/usePopcorn";
import { Loader } from "../Loader";
import { PopcornError } from "./PopcornError";

type PopcornStatusGuardProps = {
  children: ReactNode;
};

export function PopcornStatusGuard({ children }: PopcornStatusGuardProps) {
  const { popcornStatus, reinitializePopcorn } = usePopcorn();

  if (popcornStatus === "loading") {
    return <Loader message="Popcorn is loading..." />;
  }

  if (popcornStatus === "error") {
    return <PopcornError onRetry={reinitializePopcorn} />;
  }

  return children;
}
