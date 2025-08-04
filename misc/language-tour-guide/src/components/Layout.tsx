import { Outlet } from "react-router";
import Header from "./Header";

function Layout() {
  return (
    <>
      <main className="pt-16 font-inter lg:min-h-dvh flex flex-col max-h-screen">
        <Header />
        <Outlet />
      </main>
    </>
  );
}

export default Layout;
