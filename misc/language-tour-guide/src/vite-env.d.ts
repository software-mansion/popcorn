/// <reference types="vite/client" />
/// <reference types="vite-plugin-svgr/client" />

declare module "*.livemd" {
  const content: string;
  export default content;
}
