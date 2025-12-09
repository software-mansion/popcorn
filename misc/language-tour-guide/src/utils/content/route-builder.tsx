import { Route } from "react-router";
import { Suspense, type JSX } from "react";
import { getSlug, navigationConfig } from "./configuration";
import { isConfigGroup, type NavigationConfig, type RouteItem } from "./types";
import { Loader } from "../../components/Loader";

const MAX_NAVIGATION_DEPTH = 5000;

function buildRoutes(
  config: NavigationConfig,
  parentPath: string = "",
  depth: number = 0
): RouteItem[] {
  if (depth >= MAX_NAVIGATION_DEPTH) {
    console.warn(
      `Route depth limit reached (${MAX_NAVIGATION_DEPTH}). Stopping recursion to prevent stack overflow.`
    );
    return [];
  }

  return config.flatMap((item) => {
    const slug = getSlug(item);

    if (isConfigGroup(item)) {
      const currentPath = parentPath ? `${parentPath}/${slug}` : slug;
      return buildRoutes(item.items, currentPath, depth + 1);
    } else {
      const currentPath = parentPath ? `${parentPath}/${slug}` : slug;
      return {
        path: currentPath,
        component: item.component
      };
    }
  });
}

export const routes = buildRoutes(navigationConfig);

export function createRouteComponents(): JSX.Element[] {
  return routes.map((route) => {
    return (
      <Route
        path={route.path}
        element={
          <Suspense key={route.path} fallback={<Loader />}>
            <route.component />
          </Suspense>
        }
      />
    );
  });
}
