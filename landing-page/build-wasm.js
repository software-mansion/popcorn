import { spawn } from "child_process";
import { cp, rm, mkdir, readdir } from "fs/promises";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

/**
 * @param {{ dir: string, wasmSrcPathDefault?: string, newBundleName: string, extraEnv?: Record<string, string> }} options
 */
export function buildBundle({ dir, wasmSrcPathDefault, newBundleName, extraEnv = {} }) {
  return {
    name: "build-bundle",
    hooks: {
      "astro:config:setup": async ({ logger, config }) => {
        logger.info(`Building bundle in '${dir}'...`);
        const wasmSrcPath = wasmSrcPathDefault ?? join(dir, "dist", "wasm");
        const wasmDestPath = wasmDir(config);

        await run("mix", ["build"], { dir, env: extraEnv });

        const srcFiles = await readdir(wasmSrcPath);
        const [avm] = srcFiles.filter((path) => path.endsWith(".avm"));
        const src = join(wasmSrcPath, avm);
        const dst = join(wasmDestPath, newBundleName);

        await cp(src, dst);

        logger.info("Bundle copied");
      },
    },
  };
}

export function cleanWasmDir() {
  return {
    name: "clean-wasm",
    hooks: {
      "astro:config:setup": async ({ logger, config }) => {
        logger.info(`Cleaning wasm directory...`);
        const wasmDestPath = wasmDir(config);

        await rm(wasmDestPath, { force: true, recursive: true });
        await mkdir(wasmDestPath);

        logger.info("Wasm directory cleaned");
      },
    },
  };
}

function wasmDir(config) {
  const publicDir = config.publicDir;
  const publicPath = fileURLToPath(publicDir);
  return join(publicPath, "wasm");
}

function run(cmd, args, { dir, env = {} }) {
  const strCmd = `${cmd} ${args.join(" ")}`;

  return new Promise((resolve, reject) => {
    const child = spawn(cmd, args, {
      cwd: dir,
      stdio: "inherit",
      env: { ...process.env, ...env },
    });

    child.on("close", (code) => {
      const ok = code === 0;

      if (ok) {
        resolve();
      } else {
        reject(new Error(`${strCmd} failed with code ${code}`));
      }
    });
  });
}
