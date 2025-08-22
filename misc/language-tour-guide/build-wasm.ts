import { spawn } from "child_process";
import { appendFile } from "fs/promises";
import { join } from "path";
import type { ResolvedConfig } from "vite";

function run(cmd: string, args: string[], dir: string) {
  const strCmd = `${cmd} ${args.join(" ")}`;
  return new Promise((resolve, reject) => {
    const child = spawn(cmd, args, {
      cwd: dir,
      stdio: "inherit",
    });
    child.on("close", (code) => {
      const ok = code === 0;
      if (ok) {
        resolve(ok);
      } else {
        reject(new Error(`${strCmd} failed with code ${code}`));
      }
    });
  });
}

export function updatePopcorn() {
  let publicDir: string;
  return {
    name: "update-popcorn",
    configResolved(config: ResolvedConfig) {
      publicDir = config.publicDir;
    },
    async buildStart() {
      try {
        await run("mix", ["deps.get"], "./elixir_tour");
        await run("mix", ["popcorn.cook"], "./elixir_tour");
      } catch (err) {
        console.error("[update-popcorn] Command failed:", err);
        throw err; // Optionally rethrow to stop the build
      }

      const popcornScriptPath = join(publicDir, "wasm", "popcorn.js");
      try {
        await appendFile(popcornScriptPath, "\nwindow.Popcorn = Popcorn;\n");
        console.log(`[modify-popcorn-script] Appended to ${popcornScriptPath}`);
      } catch (err) {
        console.error(
          `[update-popcorn] Could not append to ${popcornScriptPath}:`,
          err
        );
        throw err;
      }
    },
  };
}
