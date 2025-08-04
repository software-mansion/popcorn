import CodeEditor from "./CodeEditor/CodeEditor";
import Description from "./Description";
import Results from "./Results";

interface ExampleLayoutProps {
  children: React.ReactNode;
}

function ExampleLayout({ children }: ExampleLayoutProps) {
  return (
    <div className="text-brown-100 gird-rows-[1fr_1fr_1fr] bg-light-20 grid h-full grow gap-x-4 gap-y-3 lg:min-h-0 lg:grid-cols-2 lg:grid-rows-[70%_28%] lg:gap-y-4">
      <Description>{children}</Description>
      <CodeEditor />
      <Results />
    </div>
  );
}

export default ExampleLayout;
