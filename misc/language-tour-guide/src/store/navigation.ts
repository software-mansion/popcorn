import { create } from "zustand";
import { createNavigation } from "../utils/content/navigation-builder";
import type { NavigationTree } from "../utils/content/types";

interface NavigationStore {
  navigation: NavigationTree | null;
  isLoading: boolean;
  loadNavigation: () => Promise<void>;
}

export const useNavigationStore = create<NavigationStore>((set, get) => ({
  navigation: null,
  isLoading: false,
  loadNavigation: async () => {
    if (get().navigation) return;

    set({ isLoading: true });
    try {
      const navTree = await createNavigation();
      set({ navigation: navTree, isLoading: false });
    } catch (err) {
      console.error("Failed to load navigation:", err);
      set({ isLoading: false });
    }
  }
}));
