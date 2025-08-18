import { useEffect } from "react";
import { useLocation } from "react-router";

export function useScrollToHash() {
  const { hash } = useLocation();

  useEffect(() => {
    if (hash) {
      const id = hash.slice(1);
      const el = document.getElementById(id);
      if (!el) {
        return;
      }

      el.scrollIntoView();
    } else {
      const descriptionSection = document.getElementById("description");
      if (!descriptionSection) {
        return;
      }

      descriptionSection.scrollTo({ top: 0 });
    }
  }, [hash]);
}
