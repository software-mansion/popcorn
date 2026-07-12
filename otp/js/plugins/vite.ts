import { cp, mkdir, readFile, rm } from "node:fs/promises";
import { dirname, isAbsolute, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import type { IncomingMessage, ServerResponse } from "node:http";
import type { Plugin, ResolvedConfig } from "vite";
import { popcorn as prepare, type Options, type Prepared } from "./shared";

const CORS_HEADERS = {
  "Cross-Origin-Opener-Policy": "same-origin",
  "Cross-Origin-Embedder-Policy": "require-corp",
};

// Plugin is at dist/plugins/vite.mjs, dist/ is one level up.
const distDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");

type Res = ServerResponse<IncomingMessage>;

export function popcorn(options: Options): Plugin {
  let prepared: Prepared | undefined;
  let repacking: Promise<Prepared> | undefined;
  let dirty = false;
  let outDir: string | undefined;

  // Dev/preview: pack once at first request, re-pack lazily after the app's
  // compiled beams change. A single in-flight promise dedupes concurrent
  // requests; a change during a re-pack re-sets `dirty` so it isn't lost.
  const ensurePrepared = (): Promise<Prepared> => {
    if (prepared !== undefined && !dirty) return Promise.resolve(prepared);
    if (repacking === undefined) {
      dirty = false;
      repacking = prepare(options)
        .then(async (next) => {
          if (prepared !== undefined) {
            await rm(prepared.dir, { recursive: true, force: true });
          }
          prepared = next;
          return next;
        })
        .finally(() => {
          repacking = undefined;
        });
    }
    return repacking;
  };

  const cleanup = () => {
    if (prepared !== undefined) {
      rm(prepared.dir, { recursive: true, force: true });
      prepared = undefined;
    }
  };

  const serve = async (
    req: IncomingMessage,
    res: Res,
    next: (error?: unknown) => void,
  ) => {
    const url = req.url;
    if (url === undefined || !url.startsWith("/otp-assets/")) {
      next();
      return;
    }

    let dir: string;
    try {
      ({ dir } = await ensurePrepared());
    } catch (error) {
      next(error);
      return;
    }

    const assetsRoot = join(dir, "otp-assets");
    const rel = url.slice("/otp-assets/".length).split("?")[0];
    const filePath = resolve(assetsRoot, rel);
    if (!isUnder(assetsRoot, filePath)) {
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
    name: "popcorn-otp",

    config() {
      return {
        // Exclude from prebundling so import.meta.url resolves correctly.
        optimizeDeps: { exclude: ["@swmansion/popcorn-otp"] },
        // COOP/COEP must be on every response (SharedArrayBuffer/pthreads),
        // including the worker/beam files Vite serves from the package itself.
        server: { headers: CORS_HEADERS },
        preview: { headers: CORS_HEADERS },
      };
    },

    configResolved(config: ResolvedConfig) {
      config.server.fs.allow.push(distDir);
      outDir = resolve(config.root, config.build.outDir);
    },

    configureServer(server) {
      const rootDir = resolve(options.rootDir);
      server.watcher.add(rootDir);
      server.watcher.on("all", (_event, file) => {
        if (isUnder(rootDir, file)) dirty = true;
      });
      server.middlewares.use(serve);
      server.httpServer?.once("close", cleanup);
    },

    configurePreviewServer(server) {
      server.middlewares.use(serve);
      server.httpServer?.once("close", cleanup);
    },

    async closeBundle() {
      assert(outDir !== undefined, "outDir was not resolved");
      const built = await prepare(options);
      try {
        await mkdir(outDir, { recursive: true });
        await cp(built.dir, outDir, { recursive: true });
      } finally {
        await rm(built.dir, { recursive: true, force: true });
      }
    },
  };
}

function isUnder(dir: string, file: string): boolean {
  const rel = relative(dir, file);
  return rel !== "" && !rel.startsWith("..") && !isAbsolute(rel);
}

function setHeaders(res: Res): void {
  for (const [key, value] of Object.entries(CORS_HEADERS)) {
    res.setHeader(key, value);
  }
}

function setContentType(res: Res, path: string): void {
  if (path.endsWith(".mjs")) {
    res.setHeader("Content-Type", "text/javascript");
  } else if (path.endsWith(".wasm")) {
    res.setHeader("Content-Type", "application/wasm");
  } else if (path.endsWith(".json")) {
    res.setHeader("Content-Type", "application/json");
  } else {
    res.setHeader("Content-Type", "application/octet-stream");
  }
}

function assert(ok: boolean, message: string): asserts ok {
  if (!ok) {
    throw new Error(`[popcorn-otp] ${message}`);
  }
}
