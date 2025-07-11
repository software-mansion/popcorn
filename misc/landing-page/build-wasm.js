import { spawn } from "child_process";
import { cp, rename, appendFile } from "fs/promises";
import { join } from "path";
import { fileURLToPath } from "url";

export default function buildWasm({ dir }) {
  return {
    name: "build-wasm",
    hooks: {
      "astro:config:setup": async ({ logger, config }) => {
        logger.info("Building Wasm project...");

        // Run mix popcorn.cook in the specified directory
        await new Promise((resolve, reject) => {
          const child = spawn("mix", ["popcorn.cook"], {
            cwd: dir,
            stdio: "inherit",
          });
          child.on("close", (code) => {
            if (code === 0) {
              resolve();
            } else {
              reject(new Error(`mix popcorn.cook failed with code ${code}`));
            }
          });
        });

        // Get public directory path
        const publicDir = config.publicDir;
        const publicPath = fileURLToPath(publicDir);

        // Copy static/wasm/ directory
        const wasmSrcPath = join(dir, "static", "wasm");
        const wasmDestPath = join(publicPath, "wasm");
        await cp(wasmSrcPath, wasmDestPath, { recursive: true });

        // await rename(
        //   join(wasmDestPath, "popcorn.js"),
        //   join(wasmDestPath, "popcorn.mjs"),
        // );
        // await rename(
        //   join(wasmDestPath, "popcorn_iframe.js"),
        //   join(wasmDestPath, "popcorn_iframe.mjs"),
        // );
        await appendFile(
          join(wasmDestPath, "popcorn.js"),
          "\nwindow.Popcorn = Popcorn;\n",
        );

        logger.info("Wasm files copied to public directory");
      },
    },
  };
}
