import { MDXProvider } from "@mdx-js/react";
import { components } from "./markdown";
import NavigationBar from "./navigation/NavigationBar";
import { usePopcorn } from "../utils/hooks/usePopcorn";
import { Loader } from "./Loader";
import { Outlet } from "react-router";

export function Description() {
  const { isLoadingPopcorn } = usePopcorn();

  return (
    <section
      id="description"
      className="bg-light-20 text-brown-100 scrollbar relative flex h-full w-full grow flex-col"
    >
      <div className="flex max-w-[1024px] grow flex-col p-8 pt-8 wrap-break-word lg:mx-auto lg:px-16">
        {isLoadingPopcorn ? (
          <Loader message="Popcorn is loading..." />
        ) : (
          <MDXProvider components={components}>
            <Outlet />
          </MDXProvider>
        )}
      </div>
      <NavigationBar />
    </section>
  );
}
