import { defineConfig } from "vite";
import { popcorn } from "@swmansion/popcorn/vite";

// https://vite.dev/config/
export default defineConfig({
  base: "/",
  plugins: [
    popcorn({ bundlePath: "./public/bundle.avm" }),
  ],
  server: {
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp"
    }
  }
});
