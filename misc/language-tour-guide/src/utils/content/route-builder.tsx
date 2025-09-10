import { lazy, Suspense, type JSX } from "react";
import { Route } from "react-router";
import { Loader } from "../../components/Loader";
import { getNavigationPath, mdxModules } from "./mdx-loader";
import { MdxWrapper } from "../../components/markdown/MdxWrapper";

export function createRouteComponents(): JSX.Element[] {
  return Object.entries(mdxModules).map(([path, component]) => {
    const routePath = getNavigationPath(path);

    const LazyComponent = lazy(async () => {
      const module = await component();

      return {
        default: () => (
          <MdxWrapper code={module.defaultCode} Component={module.default} />
        )
      };
    });

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
