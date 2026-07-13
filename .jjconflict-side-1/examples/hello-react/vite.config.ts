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

    popcorn({ rootDir: "./ex_app", app: "ex_app" }),
  ],
});
