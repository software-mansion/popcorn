import { useEffect } from "react";
import { useLocation } from "react-router";

export function ScrollToHash() {
  const { hash } = useLocation();

  useEffect(() => {
    if (hash) {
      const id = hash.slice(1);
      const el = document.getElementById(id);
      if (el) {
        setTimeout(() => {
          el.scrollIntoView();
        }, 100);
      }
    } else {
      const descriptionSection = document.getElementById("description");
      if (!descriptionSection) {
        return;
      }

      descriptionSection.scrollTo({ top: 0 });
    }
  }, [hash]);

  return null;
}
