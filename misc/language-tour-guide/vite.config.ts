import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import mdx from "@mdx-js/rollup";
import tailwindcss from "@tailwindcss/vite";
import { updatePopcorn } from "./build-wasm";
import svgr from "vite-plugin-svgr";
import rehypeHighlight from "rehype-highlight";
import elixir from "highlight.js/lib/languages/elixir";
import { rehypeRawCode } from "./src/plugins/rehypeRawCode";
import { remarkCollectCode } from "./src/plugins/remarkCollectCode";
import { livemdPlugin } from "./src/plugins/livemd";

// https://vite.dev/config/
export default defineConfig({
  base: "/",
  plugins: [
    react(),
    tailwindcss(),
    livemdPlugin(),
    mdx({
      providerImportSource: "@mdx-js/react",
      remarkPlugins: [remarkCollectCode],
      rehypePlugins: [
        rehypeRawCode,
        [rehypeHighlight, { languages: { elixir } }]
      ]
    }),
    updatePopcorn(),
    svgr()
  ],
  server: {
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp"
    }
  }
});
