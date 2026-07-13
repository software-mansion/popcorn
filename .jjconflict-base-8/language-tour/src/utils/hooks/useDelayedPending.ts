import { useEffect, useState } from "react";

export function useDelayedPending(
  isExecuting: boolean,
  delayMs = 300
): boolean {
  const [showPending, setShowPending] = useState(false);

  useEffect(() => {
    if (!isExecuting) {
      setShowPending(false);
      return;
    }

    const timer = setTimeout(() => setShowPending(true), delayMs);
    return () => clearTimeout(timer);
  }, [isExecuting, delayMs]);

  return showPending;
}
