import { useMemo } from "react";
import { useLocation } from "react-router";
import { useNavigationStore } from "../../store/navigation";
import { getNodeNavigationSiblings } from "../content/navigation-builder";

export function useSiblingsNode() {
  const { pathname } = useLocation();
  const navigation = useNavigationStore((state) => state.navigation);

  const navigationProperty = useMemo(() => {
    if (!navigation) {
      return {
        siblingsNode: null,
        childrenCount: 0,
        currentIndex: 0
      };
    }
    const result = getNodeNavigationSiblings(navigation, pathname);
    return result;
  }, [navigation, pathname]);

  return navigationProperty;
}
