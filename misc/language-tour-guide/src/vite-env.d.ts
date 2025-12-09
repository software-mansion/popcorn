/// <reference types="vite/client" />
/// <reference types="vite-plugin-svgr/client" />

declare module "*.livemd" {
  import type { ComponentType } from "react";
  import type { CodeSnippet } from "./plugins/livemd/parser";

  export const codeSnippets: CodeSnippet[];
  const Component: ComponentType;
  export default Component;
}
