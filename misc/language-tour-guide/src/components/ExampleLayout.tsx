import { CodeEditor } from "./code-editor/CodeEditor";
import { Description } from "./Description";
import { Results } from "./execution/Results";
import { Outlet } from "react-router";

export function ExampleLayout() {
  return (
    <div className="text-brown-100 bg-light-20 flex h-full grow flex-col gap-x-4 gap-y-3 lg:grid lg:min-h-0 lg:grid-cols-2 lg:grid-rows-[60%_38%] lg:gap-y-4">
      <Description>
        <Outlet />
      </Description>
      <CodeEditor />
      <Results />
    </div>
  );
}
