import { Outlet } from "react-router";
import { Header } from "./Header";
import { Navigation } from "./navigation/Navigation";

export function Layout() {
  return (
    <main className="font-inter flex max-h-screen flex-col pt-16 lg:min-h-dvh">
      <Header />
      <Navigation />
      <Outlet />
    </main>
  );
}
