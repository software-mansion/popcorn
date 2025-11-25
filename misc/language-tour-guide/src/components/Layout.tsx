import { Outlet } from "react-router";
import { Header } from "./Header";
import { Navigation } from "./navigation/Navigation";

export function Layout() {
  return (
    <main className="font-inter flex max-h-screen min-h-dvh flex-col pt-16">
      <Header />
      <Navigation />
      <Outlet />
    </main>
  );
}
