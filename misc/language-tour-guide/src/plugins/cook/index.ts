import type { Plugin } from "vite";
import { exec } from "node:child_process";
import { promisify } from "node:util";

import path from "node:path";

const execPromise = promisify(exec);

export function cookOnChange(): Plugin {
  return {
    name: "vite-plugin-cook-on-change",
    apply: "serve",
    configureServer(server) {
      const elixirTourDir = path.join(process.cwd(), "elixir_tour");
      const elixirTourLib = path.join(elixirTourDir, "lib");

      server.watcher.add(elixirTourLib);

      server.watcher.on("change", async (file) => {
        if (!file.startsWith(elixirTourLib) || !file.endsWith(".ex")) return;

        console.log(`[cook] ${path.basename(file)} changed`);
        try {
          await execPromise("mix popcorn.cook", {
            cwd: elixirTourDir
          });
        } catch (error) {
          console.error("[cook] mix popcorn.cook failed", error);
        }
      });
    }
  };
}
