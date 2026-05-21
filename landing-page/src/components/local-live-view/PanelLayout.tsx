import type { PropsWithChildren } from "react";

type PanelHeaderProps = {
  title: string;
};

export function PanelLayout({
  children,
  title,
}: PropsWithChildren<PanelHeaderProps>) {
  return (
    <div className="bg-llv-bg-100 rounded-xl px-5 py-3.5 pr-0">
      <p className="text-llv-brown-50 mb-2.5 font-mono text-xs font-semibold tracking-widest uppercase">
        {title}
      </p>
      {children}
    </div>
  );
}
