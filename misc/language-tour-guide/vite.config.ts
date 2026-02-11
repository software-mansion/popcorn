import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import mdx from "@mdx-js/rollup";
import tailwindcss from "@tailwindcss/vite";
import svgr from "vite-plugin-svgr";
import rehypeHighlight from "rehype-highlight";
import elixir from "highlight.js/lib/languages/elixir";
import { livemdPlugin } from "./src/plugins/livemd";
import { popcorn } from "@swmansion/popcorn/vite";

// https://vite.dev/config/
export default defineConfig({
  base: "/",
  plugins: [
    react(),
    tailwindcss(),
    livemdPlugin(),
    mdx({
      providerImportSource: "@mdx-js/react",
      rehypePlugins: [[rehypeHighlight, { languages: { elixir } }]]
    }),
    svgr(),
    popcorn({ bundlePath: "./public/wasm/bundle.avm" })
  ],
  server: {
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp"
    }
  }
});
