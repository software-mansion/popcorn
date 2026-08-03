import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { atomvm } from "@swmansion/popcorn/atomvm/vite";

// https://vite.dev/config/
export default defineConfig({
  plugins: [
    react({
      babel: {
        plugins: [["babel-plugin-react-compiler"]],
      },
    }),

    atomvm({
      // Path to your .avm bundle file
      bundlePaths: ["ex_app/_build/wasm/bundle.avm"],
    }),
  ],
});
