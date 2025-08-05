import type { PropsWithChildren } from "react";

export function Description({ children }: PropsWithChildren) {
  return (
    <section className="bg-light-30 border-grey-20 scrollbar h-full border p-4 wrap-break-word lg:row-span-2 lg:overflow-y-auto lg:p-8">
      {children}
    </section>
  );
}
