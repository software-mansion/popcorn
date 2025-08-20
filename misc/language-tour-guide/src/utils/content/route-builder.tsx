import { lazy, Suspense, type JSX } from "react";
import { Route } from "react-router";
import { Loader } from "../../components/Loader";
import { getRoutePath, mdxModules } from "./mdx-loader";

export function createRouteComponents(): JSX.Element[] {
  return Object.entries(mdxModules).map(([path, component]) => {
    const routePath = getRoutePath(path);

    const LazyComponent = lazy(() => component());

    return (
      <Route
        key={routePath}
        path={routePath}
        element={
          <Suspense fallback={<Loader />}>
            <LazyComponent />
          </Suspense>
        }
      />
    );
  });
}
