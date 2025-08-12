import { MDXProvider } from "@mdx-js/react";
import type { PropsWithChildren } from "react";
import { components } from "./markdown";

export function Description({ children }: PropsWithChildren) {
  return (
    <section
      id="description"
      className="bg-light-30 border-grey-20 scrollbar h-full w-full border p-4 wrap-break-word lg:row-span-2 lg:overflow-y-auto lg:p-8"
    >
      <MDXProvider components={components}>{children}</MDXProvider>
    </section>
  );
}
