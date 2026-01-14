import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { popcorn } from "@swmansion/popcorn/vite";

// https://vite.dev/config/
export default defineConfig({
  plugins: [
    react({
      babel: {
        plugins: [["babel-plugin-react-compiler"]],
      },
    }),

    popcorn({
      // Path to your .avm bundle file
      bundlePath: "ex_app/_build/wasm/bundle.avm",
    }),
  ],
});
