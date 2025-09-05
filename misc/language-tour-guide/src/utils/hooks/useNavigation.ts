import { useEffect } from "react";
import { useNavigationStore } from "../../store/navigation";

export function useNavigation() {
  const { navigation, isLoading, loadNavigation } = useNavigationStore();

  useEffect(() => {
    loadNavigation();
  }, [loadNavigation]);

  return { navigation, isLoading };
}
