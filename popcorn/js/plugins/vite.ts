import { readFile, stat } from "fs/promises";
import { basename, dirname, resolve } from "path";
import { fileURLToPath } from "url";
import { type PopcornPluginOptions } from "./shared";

import type { IncomingMessage, ServerResponse } from "http";
import type { Plugin, ResolvedConfig } from "vite";

const __dirname = dirname(fileURLToPath(import.meta.url));
// Plugin is at dist/plugins/vite.mjs, dist/ is one level up
const popcornDistDir = resolve(__dirname, "..");

export function popcorn(options: PopcornPluginOptions): Plugin {
  const bundlePaths = options.bundlePaths;
  const bundles = bundlePaths.map((p) => ({
    path: p,
    name: basename(p),
    url: `/${basename(p)}`,
  }));

  let assetsDir: string;

  return {
    name: "popcorn",

    config() {
      return {
        // Exclude popcorn from prebundling so import.meta.url resolves correctly
        optimizeDeps: {
          exclude: ["@swmansion/popcorn"],
        },
      };
    },

    async configResolved(config: ResolvedConfig) {
      const results = await Promise.allSettled(
        bundles.map((b) => stat(b.path)),
      );
      for (const [i, result] of results.entries()) {
        if (result.status === "rejected") {
          this.error(`[popcorn] Bundle doesn't exist at '${bundles[i].path}'`);
        }
      }

      config.server.fs.allow.push(popcornDistDir);
      assetsDir = config.build.assetsDir;
    },

    configureServer(server) {
      server.middlewares.use(async (req, res, next) => {
        setSharedArrayBufferHeaders(res);

        for (const bundle of bundles) {
          const opts = { bundleUrl: bundle.url, bundlePath: bundle.path };
          const served = await serveAvmBundle(req, res, opts);
          if (served) return;
        }

        next();
      });
    },

    configurePreviewServer(server) {
      server.middlewares.use(async (req, res, next) => {
        setSharedArrayBufferHeaders(res);

        for (const bundle of bundles) {
          const opts = { bundleUrl: bundle.url, bundlePath: bundle.path };
          const served = await serveAvmBundle(req, res, opts);
          if (served) return;
        }

        next();
      });
    },

    async generateBundle() {
      // Emit bundles to assets directory
      for (const bundle of bundles) {
        this.emitFile({
          type: "asset",
          fileName: `${assetsDir}/${bundle.name}`,
          source: await readFile(bundle.path),
        });
      }

      // Emit AtomVM files to assets directory (same location as iframe.mjs)
      // Vite treats iframe.mjs as a static asset and doesn't analyze its imports
      for (const name of ["AtomVM.mjs", "AtomVM.wasm"]) {
        this.emitFile({
          type: "asset",
          fileName: `${assetsDir}/${name}`,
          source: await readFile(resolve(popcornDistDir, name)),
        });
      }
    },
  };
}

type Res = ServerResponse<IncomingMessage>;

type ServeAvmBundleOpts = { bundleUrl: string; bundlePath: string };
async function serveAvmBundle(
  req: IncomingMessage,
  res: Res,
  { bundleUrl, bundlePath }: ServeAvmBundleOpts,
) {
  try {
    if (req.url === bundleUrl) {
      const content = await readFile(bundlePath);
      res.setHeader("Content-Type", "application/octet-stream");
      res.end(content);
      return true;
    }
    return false;
  } catch (err) {
    console.error(`[popcorn] Failed to serve bundle:`, err);
    throw err;
  }
}

function setSharedArrayBufferHeaders(res: Res): void {
  res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
  res.setHeader("Cross-Origin-Embedder-Policy", "require-corp");
}
