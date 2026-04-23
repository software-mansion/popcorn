import { readFile } from "fs/promises";
import { resolve, dirname, relative, isAbsolute } from "path";
import { fileURLToPath } from "url";
import { defineConfig, type Plugin } from "vite";

import type { IncomingMessage, ServerResponse } from "http";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const otpJsRootDir = resolve(__dirname, "../..");
const assetsDir = resolve(otpJsRootDir, "dist/assets");
const CORS_HEADERS = {
  "Cross-Origin-Opener-Policy": "same-origin",
  "Cross-Origin-Embedder-Policy": "require-corp",
};

type Res = ServerResponse<IncomingMessage>;

function otpAssetsPlugin(): Plugin {
  const serveAsset = async (
    req: IncomingMessage,
    res: Res,
    next: () => void,
  ) => {
    const requestUrl = req.url;
    if (requestUrl === undefined || !requestUrl.startsWith("/otp-assets/")) {
      next();
      return;
    }

    const relativePath = requestUrl.slice("/otp-assets/".length);
    const filePath = resolve(assetsDir, relativePath);
    const assetRelativePath = relative(assetsDir, filePath);
    if (assetRelativePath.startsWith("..") || isAbsolute(assetRelativePath)) {
      res.statusCode = 403;
      setHeaders(res);
      res.end("Forbidden");
      return;
    }

    try {
      const content = await readFile(filePath);
      setHeaders(res);
      setContentType(res, filePath);
      res.end(content);
    } catch {
      res.statusCode = 404;
      setHeaders(res);
      res.end("Not found");
    }
  };

  return {
    name: "otp-assets",
    configureServer(server) {
      server.middlewares.use(serveAsset);
    },
    configurePreviewServer(server) {
      server.middlewares.use(serveAsset);
    },
  };
}

function setHeaders(res: Res) {
  for (const [key, value] of Object.entries(CORS_HEADERS)) {
    res.setHeader(key, value);
  }
}

function setContentType(res: Res, path: string) {
  if (path.endsWith(".mjs")) {
    res.setHeader("Content-Type", "text/javascript");
    return;
  }
  if (path.endsWith(".wasm")) {
    res.setHeader("Content-Type", "application/wasm");
    return;
  }
  if (path.endsWith(".json")) {
    res.setHeader("Content-Type", "application/json");
    return;
  }
  res.setHeader("Content-Type", "application/octet-stream");
}

export default defineConfig({
  root: __dirname,
  plugins: [otpAssetsPlugin()],
  server: {
    host: "127.0.0.1",
    port: 5173,
    strictPort: true,
    headers: CORS_HEADERS,
    fs: {
      allow: [otpJsRootDir],
    },
  },
  preview: {
    host: "127.0.0.1",
    port: 5173,
    strictPort: true,
    headers: CORS_HEADERS,
  },
  optimizeDeps: {
    exclude: ["@swmansion/popcorn-otp"],
  },
});
