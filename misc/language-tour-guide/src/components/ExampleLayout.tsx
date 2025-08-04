import CodeEditor from "./CodeEditor/CodeEditor";
import Description from "./Description";
import Results from "./Results";

interface ExampleLayoutProps {
  children: React.ReactNode;
}

function ExampleLayout({ children }: ExampleLayoutProps) {
  return (
    <div className="grid text-brown-100 gird-rows-[1fr_1fr_1fr] lg:grid-cols-2 lg:grid-rows-[70%_28%] gap-x-4 lg:gap-y-4 gap-y-3 grow bg-light-20 h-full lg:min-h-0">
      <Description>{children}</Description>
      <CodeEditor />
      <Results />
    </div>
  );
}

export default ExampleLayout;
