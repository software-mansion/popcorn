import { useState, useEffect } from "react";
import { createNavigation } from "../content/navigation-builder";
import type { NavigationTree } from "../content/types";

export function useNavigation() {
  const [navigation, setNavigation] = useState<NavigationTree | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    async function loadNavigation() {
      try {
        const navTree = await createNavigation();
        setNavigation(navTree);
      } catch (err) {
        console.error("Failed to load navigation:", err);
      } finally {
        setIsLoading(false);
      }
    }

    loadNavigation();
  }, []);

  return { navigation, isLoading };
}
