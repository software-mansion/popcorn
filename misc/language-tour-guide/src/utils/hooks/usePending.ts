import { useCallback, useState } from "react";

export function usePending() {
  const [pending, setPending] = useState<boolean>(false);
  const withPending = useCallback(async (fn: () => Promise<void>) => {
    setPending(true);
    try {
      await fn();
    } finally {
      setPending(false);
    }
  }, []);

  return [pending, withPending] as const;
}
