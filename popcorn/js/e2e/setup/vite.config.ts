import { resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { defineConfig, type Plugin } from "vite";
import { popcorn } from "@swmansion/popcorn/vite";

/** The echo endpoint used by the runtime fetch tests. */
function httpEndpoints(): Plugin {
  return {
    name: "e2e-http-endpoints",
    configureServer(server) {
      server.middlewares.use("/echo", (req, res) => {
        const chunks: Buffer[] = [];
        req.on("data", (chunk: Buffer) => chunks.push(chunk));
        req.on("end", () => {
          res.setHeader("content-type", "application/octet-stream");
          res.end(Buffer.concat(chunks));
        });
      });
    },
  };
}

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export default defineConfig({
  root: __dirname,
  plugins: [
    popcorn({
      rootDir: resolve(__dirname, "entrypoint-app"),
      app: "test_entrypoint",
    }),
    httpEndpoints(),
  ],
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
