import { resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { defineConfig, type Plugin } from "vite";
import { popcorn } from "@swmansion/popcorn/vite";

/** The echo endpoint used by the runtime fetch tests. */
function httpEndpoints(): Plugin {
  return {
    name: "e2e-http-endpoints",
    configureServer(server) {
      server.middlewares.use("/req/get", (_req, res) => {
        res.statusCode = 206;
        res.setHeader("content-type", "text/plain");
        res.setHeader("x-popcorn-fixture", "get");
        res.write("browser-");
        setTimeout(() => res.end("stream"), 10);
      });

      server.middlewares.use("/req/post", (req, res) => {
        const chunks: Buffer[] = [];
        req.on("data", (chunk: Buffer) => chunks.push(chunk));
        req.on("end", () => {
          res.statusCode = 201;
          res.setHeader("content-type", "application/octet-stream");
          res.setHeader("x-popcorn-fixture", "post");
          res.end(Buffer.concat(chunks));
        });
      });

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
    }),
    httpEndpoints(),
  ],
  server: {
    host: "127.0.0.1",
    port: 5173,
    strictPort: true,
  },
});
