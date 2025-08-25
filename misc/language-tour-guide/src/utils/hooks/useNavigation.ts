import { useState, useEffect } from "react";
import {
  createNavigation,
  getNodeNavigationSiblings
} from "../content/navigation-builder";
import type { NavigationTree } from "../content/types";
import { useLocation } from "react-router";

export function useNavigation() {
  const { pathname } = useLocation();

  const [navigation, setNavigation] = useState<NavigationTree | null>(null);

  const [siblingsNode, setSiblingsNode] = useState<{
    previousNode: { title: string; path: string } | null;
    nextNode: { title: string; path: string } | null;
  }>({
    previousNode: null,
    nextNode: null
  });

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

  useEffect(() => {
    if (!navigation) return;
    const siblingsNode = getNodeNavigationSiblings(navigation, pathname);

    setSiblingsNode(siblingsNode);
  }, [navigation, pathname]);

  return { navigation, isLoading, siblingsNode };
}
