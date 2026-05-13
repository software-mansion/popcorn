import type { Plugin } from "vite";

const ROUTE = "/cookie-script.js";
const UPSTREAM =
  "https://cdn.cookie-script.com/s/19b5b47f7a8f2606f861864571339358.js";

async function fetchScript(): Promise<string> {
  const res = await fetch(UPSTREAM);
  if (!res.ok) {
    throw new Error(
      `[cookie-script-proxy] failed to fetch ${UPSTREAM}: ${res.status} ${res.statusText}`
    );
  }
  return res.text();
}

export function cookieScriptProxy(): Plugin {
  return {
    name: "vite-plugin-cookie-script-proxy",
    configureServer(server) {
      server.middlewares.use(ROUTE, async (_req, res, next) => {
        try {
          const body = await fetchScript();
          res.setHeader("Content-Type", "application/javascript; charset=utf-8");
          res.setHeader("Cross-Origin-Resource-Policy", "cross-origin");
          res.end(body);
        } catch (err) {
          next(err as Error);
        }
      });
    },
    async generateBundle() {
      const source = await fetchScript();
      this.emitFile({
        type: "asset",
        fileName: "cookie-script.js",
        source
      });
    }
  };
}
