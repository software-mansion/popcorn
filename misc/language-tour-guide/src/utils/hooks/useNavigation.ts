import { useState, useEffect } from "react";
import { createNavigation, type NavigationTree } from "../content/navigation";

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
