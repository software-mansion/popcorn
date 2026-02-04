import { defineConfig } from "vite";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { popcorn } from "@swmansion/popcorn/vite";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const bundlePath = resolve(__dirname, "../elixir/_build/bundle.avm");

export default defineConfig({
  root: __dirname,
  plugins: [popcorn({ bundlePath })],
  server: {
    port: 5173,
    strictPort: true,
  },
});
