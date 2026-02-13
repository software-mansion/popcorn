import type { Plugin } from "vite";
import { execSync } from "node:child_process";
import path from "node:path";

export function cookOnChange(): Plugin {
  return {
    name: "vite-plugin-cook-on-change",
    apply: "serve",
    configureServer(server) {
      const elixirTourLib = path.join(process.cwd(), "elixir_tour", "lib");

      server.watcher.add(elixirTourLib);

      server.watcher.on("change", (file) => {
        if (!file.startsWith(elixirTourLib) || !file.endsWith(".ex")) return;

        console.log(`[cook] ${path.basename(file)} changed`);
        try {
          execSync("mix popcorn.cook", {
            cwd: path.join(process.cwd(), "elixir_tour"),
            stdio: "inherit"
          });
          server.ws.send({ type: "full-reload" });
        } catch {
          console.error("[cook] mix popcorn.cook failed");
        }
      });
    }
  };
}
