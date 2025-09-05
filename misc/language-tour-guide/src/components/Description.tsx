import { MDXProvider } from "@mdx-js/react";
import type { PropsWithChildren } from "react";
import { components } from "./markdown";
import NavigationBar from "./navigation/NavigationBar";

export function Description({ children }: PropsWithChildren) {
  return (
    <section
      id="description"
      className="bg-light-30 border-grey-20 scrollbar relative flex h-full w-full flex-col border lg:row-span-2 lg:overflow-y-auto"
    >
      <div className="grow p-4 wrap-break-word lg:p-8">
        <MDXProvider components={components}>{children}</MDXProvider>
      </div>
      <NavigationBar />
    </section>
  );
}
