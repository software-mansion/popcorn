import { Route, Routes } from "react-router";
import { Layout } from "./components/Layout";
import { ExampleLayout } from "./components/ExampleLayout";
import { createRouteComponents } from "./utils/content/content-routes";
import { useMemo } from "react";

export function AppRoutes() {
  const contentRoutes = useMemo(() => createRouteComponents(), []);

  return (
    <Routes>
      <Route element={<Layout />}>
        <Route element={<ExampleLayout />}>{contentRoutes}</Route>
      </Route>
    </Routes>
  );
}
