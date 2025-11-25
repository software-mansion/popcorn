import { Navigate, Route, Routes } from "react-router";
import { Layout } from "./components/Layout";
import { ExampleLayout } from "./components/ExampleLayout";
import { useMemo } from "react";
import { NotFound } from "./pages/NotFound";
import { createRouteComponents } from "./utils/content/route-builder";

export function AppRoutes() {
  const contentRoutes = useMemo(createRouteComponents, []);

  return (
    <Routes>
      <Route path="/" element={<Navigate to="/introduction" />} />

      <Route element={<Layout />}>
        <Route path="*" element={<NotFound />} />
        <Route element={<ExampleLayout />}>{contentRoutes}</Route>
      </Route>
    </Routes>
  );
}
