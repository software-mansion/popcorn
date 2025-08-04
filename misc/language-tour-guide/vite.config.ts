import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import mdx from "@mdx-js/rollup";
import tailwindcss from "@tailwindcss/vite";
import { updatePopcorn } from "./build-wasm";

// https://vite.dev/config/
export default defineConfig({
  plugins: [react(), tailwindcss(), mdx(), updatePopcorn()],
  server: {
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp",
      "Access-Control-Allow-Origin": "*",
    },
  },
});
