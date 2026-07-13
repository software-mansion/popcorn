import { resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { defineConfig } from "vite";
import { popcorn } from "@swmansion/popcorn-otp/vite";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const preloadAssets = process.env.PRELOAD_ASSETS !== "false";

const pluginOptions = {
  rootDir: resolve(__dirname, "entrypoint-app"),
  app: "test_entrypoint",
  ...(preloadAssets ? { preloadAssets: true } : {}),
};

export default defineConfig({
  root: __dirname,
  plugins: [popcorn(pluginOptions)],
  server: {
    host: "127.0.0.1",
    port: 5173,
    strictPort: true,
  },
  preview: {
    host: "127.0.0.1",
    port: 5173,
    strictPort: true,
  },
});
