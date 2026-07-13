import type { BannerZone } from "./shared";

export const TOP_BAR_BANNER = {
  rotateIntervalMs: 4000,
  hiddenPaths: [] as string[],
  zones: [
    {
      zoneId: "popcorn-topbar-1",
      contentId: "ea15c4216158c4097b65fe6504a4b3b7",
      fallbackBgColor: "#38acdd",
    },
    {
      zoneId: "popcorn-topbar-2",
      contentId: "ea15c4216158c4097b65fe6504a4b3b7",
      fallbackBgColor: "#38acdd",
    },
    {
      zoneId: "popcorn-topbar-3",
      contentId: "ea15c4216158c4097b65fe6504a4b3b7",
      fallbackBgColor: "#38acdd",
    },
  ] satisfies BannerZone[],
};
