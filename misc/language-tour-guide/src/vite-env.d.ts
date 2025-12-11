/// <reference types="vite/client" />
/// <reference types="vite-plugin-svgr/client" />

declare module "*.livemd" {
  import type { LiveMdModule } from "./plugins/livemd/parser";

  export const codeSnippets: LiveMdModule["codeSnippets"];
  const Component: LiveMdModule["default"];
  export default Component;
}
