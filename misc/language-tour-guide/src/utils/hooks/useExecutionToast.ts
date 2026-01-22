import { useCallback, useEffect, useState, type RefObject } from "react";
import { useIsInViewport } from "./useIsInViewport";

type UseExecutionToastOptions = {
  isExecutionSuccess: boolean;
  elementRef: RefObject<HTMLElement | null>;
};

type UseExecutionToastResult = {
  showCompleted: boolean;
  scrollToElement: () => void;
  dismissToast: () => void;
  startTracking: () => void;
};

export function useExecutionToast({
  isExecutionSuccess,
  elementRef
}: UseExecutionToastOptions): UseExecutionToastResult {
  const [isTracking, setIsTracking] = useState(false);
  const isInViewport = useIsInViewport(elementRef, { enabled: isTracking });

  const [showCompleted, setShowCompleted] = useState(false);

  const startTracking = useCallback(() => {
    setIsTracking(true);
    setShowCompleted(false);
  }, []);

  useEffect(() => {
    if (!isTracking) return;

    if (isExecutionSuccess && !isInViewport) {
      setShowCompleted(true);
    }

    if (isInViewport && isExecutionSuccess) {
      setShowCompleted(false);
      setIsTracking(false);
    }
  }, [isTracking, isExecutionSuccess, isInViewport]);

  useEffect(() => {
    if (!showCompleted) return;

    const timeout = setTimeout(() => {
      setShowCompleted(false);
      setIsTracking(false);
    }, 5000);

    return () => {
      clearTimeout(timeout);
    };
  }, [showCompleted]);

  const scrollToElement = useCallback(() => {
    setShowCompleted(false);
    setIsTracking(false);
    elementRef.current?.scrollIntoView({
      behavior: "smooth",
      block: "center"
    });
  }, [elementRef]);

  const dismissToast = useCallback(() => {
    setShowCompleted(false);
    setIsTracking(false);
  }, []);

  return {
    showCompleted,
    scrollToElement,
    dismissToast,
    startTracking
  };
}
