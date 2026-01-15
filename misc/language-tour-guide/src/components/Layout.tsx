import { Outlet } from "react-router";
import { Branding } from "./Branding";
import { Navigation } from "./navigation/Navigation";

export function Layout() {
  return (
    <>
      <header className="fixed top-0 z-30 flex w-full flex-col">
        <Branding />
        <Navigation />
      </header>
      <main className="font-inter flex max-h-screen min-h-dvh flex-col pt-28">
        <Outlet />
      </main>
    </>
  );
}
