import { Outlet } from "react-router";
import { Header } from "./Header";

export function Layout() {
  return (
    <main className="font-inter flex max-h-screen flex-col pt-16 lg:min-h-dvh">
      <Header />
      <Outlet />
    </main>
  );
}
