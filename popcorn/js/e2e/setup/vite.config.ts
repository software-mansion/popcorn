import { defineConfig } from "vite";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { atomvm } from "@swmansion/popcorn/atomvm/vite";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const bundlePath = resolve(__dirname, "../elixir/_build/bundle.avm");

export default defineConfig({
  root: __dirname,
  plugins: [atomvm({ bundlePaths: [bundlePath] })],
  server: {
    port: 5173,
    strictPort: true,
  },
});
