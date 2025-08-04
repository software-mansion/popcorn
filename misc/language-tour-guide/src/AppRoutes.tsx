import { Route, Routes } from "react-router";
import Home from "./pages/Home";
import Layout from "./components/Layout";

function AppRoutes() {
  return (
    <Routes>
      <Route element={<Layout />}>
        <Route path="/" element={<Home />} />
      </Route>
    </Routes>
  );
}

export default AppRoutes;
