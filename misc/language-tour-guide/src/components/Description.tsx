import { MDXProvider } from "@mdx-js/react";
import { components } from "./markdown";
import NavigationBar from "./navigation/NavigationBar";
import { usePopcorn } from "../utils/hooks/usePopcorn";
import { Loader } from "./Loader";
import { Outlet, useLocation } from "react-router";
import { useEffect } from "react";
import { useExecutionHistoryStore } from "../store/executionHistory";
import { useEditorsStore } from "../store/editors";

export function Description() {
  const { isLoadingPopcorn } = usePopcorn();

  const { pathname } = useLocation();
  const clearHistory = useExecutionHistoryStore((state) => state.clearHistory);
  const clearEditors = useEditorsStore((state) => state.clearEditors);

  useEffect(() => {
    clearHistory();
    clearEditors();
    window.scrollTo({ top: 0, left: 0, behavior: "instant" });
  }, [pathname, clearHistory, clearEditors]);

  return (
    <section
      id="description"
      className="bg-light-30 text-brown-100 scrollbar relative flex h-full w-full grow flex-col"
    >
      <div className="flex max-w-[1024px] grow flex-col p-8 pt-8 pb-4 wrap-break-word lg:mx-auto lg:px-16 lg:pb-8">
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
