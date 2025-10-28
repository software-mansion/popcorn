import { CodeEditor } from "./code-editor/CodeEditor";
import { Description } from "./Description";
import Execution from "./execution/Execution";
import { Outlet } from "react-router";
import { Panel, PanelGroup, PanelResizeHandle } from "react-resizable-panels";
import { useState, useEffect } from "react";

export function ExampleLayout() {
  const [isLargeScreen, setIsLargeScreen] = useState(false);

  useEffect(() => {
    // set media query with 1024px breakpoint which matches Tailwind's lg breakpoint
    const mediaQuery = window.matchMedia("(min-width: 1024px)");

    // set initial value
    setIsLargeScreen(mediaQuery.matches);

    const handleChange = (e: MediaQueryListEvent) => {
      setIsLargeScreen(e.matches);
    };

    mediaQuery.addEventListener("change", handleChange);
    return () => mediaQuery.removeEventListener("change", handleChange);
  }, []);

  if (!isLargeScreen) {
    return (
      <div className="text-brown-100 bg-light-20 flex h-full grow flex-col gap-x-4 gap-y-3">
        <Description>
          <Outlet />
        </Description>
        <CodeEditor />
        <Execution />
      </div>
    );
  }

  return (
    <PanelGroup
      direction="horizontal"
      className="text-brown-100 bg-light-20 flex h-full grow"
    >
      <Panel defaultSize={50} minSize={30}>
        <Description>
          <Outlet />
        </Description>
      </Panel>

      <PanelResizeHandle className="w-4" />

      <Panel defaultSize={50} minSize={30} className="">
        <PanelGroup direction="vertical">
          <Panel defaultSize={60} minSize={50} className="overflow-hidden">
            <CodeEditor />
          </Panel>

          <PanelResizeHandle className="h-4" />

          <Panel defaultSize={40} minSize={30} maxSize={50}>
            <Execution />
          </Panel>
        </PanelGroup>
      </Panel>
    </PanelGroup>
  );
}
