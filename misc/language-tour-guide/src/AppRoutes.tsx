import { Route, Routes } from "react-router";
import { Layout } from "./components/Layout";
import { ExampleLayout } from "./components/ExampleLayout";
import { createRouteComponents } from "./utils/content/content-routes";
import { useMemo } from "react";
import { useScrollToHash } from "./utils/hooks/useScrollToHash";
import { NotFound } from "./pages/NotFound";

export function AppRoutes() {
  useScrollToHash();
  const contentRoutes = useMemo(createRouteComponents, []);

  return (
    <Routes>
      <Route path="*" element={<NotFound />} />

      <Route element={<Layout />}>
        <Route element={<ExampleLayout />}>{contentRoutes}</Route>
      </Route>
    </Routes>
  );
}
