import { useCallback, useEffect, useRef } from "react";

export function useScrollLock(elementRef: React.RefObject<HTMLElement | null>) {
  const lockedPositionRef = useRef<number | null>(null);
  const observerRef = useRef<ResizeObserver | null>(null);
  const cleanupRef = useRef<(() => void) | null>(null);

  useEffect(() => {
    return () => {
      observerRef.current?.disconnect();
      cleanupRef.current?.();
    };
  }, []);

  const unlockScroll = useCallback(() => {
    lockedPositionRef.current = null;
    observerRef.current?.disconnect();
    observerRef.current = null;
    cleanupRef.current?.();
    cleanupRef.current = null;
  }, []);

  const adjustScroll = useCallback(() => {
    if (lockedPositionRef.current === null || !elementRef.current) return;

    const currentTop = elementRef.current.getBoundingClientRect().top;
    const difference = currentTop - lockedPositionRef.current;

    if (difference > 0) {
      window.scrollBy({ top: difference, behavior: "instant" });
    }
  }, [elementRef]);

  const lockScroll = useCallback(() => {
    if (!elementRef.current) return;

    lockedPositionRef.current = elementRef.current.getBoundingClientRect().top;

    observerRef.current?.disconnect();
    cleanupRef.current?.();

    const handleUserScroll = () => {
      unlockScroll();
    };

    window.addEventListener("wheel", handleUserScroll, { passive: true });
    window.addEventListener("touchmove", handleUserScroll, { passive: true });

    cleanupRef.current = () => {
      window.removeEventListener("wheel", handleUserScroll);
      window.removeEventListener("touchmove", handleUserScroll);
    };

    let firstInvoke = true;
    observerRef.current = new ResizeObserver(() => {
      if (firstInvoke) {
        firstInvoke = false;
        return;
      }

      adjustScroll();
    });

    const codeCells = document.querySelectorAll(".code-cell");
    codeCells.forEach((cell) => {
      observerRef.current?.observe(cell);
    });
  }, [elementRef, adjustScroll, unlockScroll]);

  return { lockScroll, unlockScroll };
}
