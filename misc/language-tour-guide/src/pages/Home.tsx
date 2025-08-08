import { ExampleLayout } from "../components/ExampleLayout";
// import Agent from "../content/processes/Agent.mdx";

import Introduction from "../content/getting-started/processes.mdx";

export function Home() {
  return (
    <>
      <ExampleLayout>
        {/* <Agent /> */}

        <Introduction />
      </ExampleLayout>
    </>
  );
}
