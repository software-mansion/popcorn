import { useEffect, useState, type RefObject } from "react";

type ObserverOption = {
  rootMargin?: string;
  enabled?: boolean;
};

export function useIsInViewport(
  ref: RefObject<HTMLElement | null>,
  options: ObserverOption = {}
): boolean {
  const { rootMargin = "-200px 0px 0px 0px", enabled = true } = options;
  const [isInViewport, setIsInViewport] = useState(true);

  useEffect(() => {
    if (!enabled) return;

    const element = ref.current;
    if (!element) return;

    const observer = new IntersectionObserver(
      ([entry]) => {
        setIsInViewport(entry.isIntersecting);
      },
      {
        threshold: 0,
        rootMargin
      }
    );

    observer.observe(element);

    return () => {
      observer.disconnect();
    };
  }, [ref, rootMargin, enabled]);

  return isInViewport;
}
