/// <reference types="vite/client" />
/// <reference types="vite-plugin-svgr/client" />

/**
 * A hash of all .livemd content files, injected at build time by the livemd plugin.
 * Used to detect when content has changed and invalidate cached editor states.
 * used in src/utils/storage/index.ts
 */
declare const __CONTENT_HASH__: string;

declare module "*.livemd" {
  import type { LiveMdModule } from "./plugins/livemd/parser";

  export const codeSnippets: LiveMdModule["codeSnippets"];
  const Component: LiveMdModule["default"];
  export default Component;
}
