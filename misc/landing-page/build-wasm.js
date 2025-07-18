import { spawn } from "child_process";
import { cp, rm, mkdir, appendFile, readdir } from "fs/promises";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

export function buildBundle({ dir, newBundleName }) {
  return {
    name: "build-bundle",
    hooks: {
      "astro:config:setup": async ({ logger, config }) => {
        logger.info(`Building bundle in '${dir}'...`);
        const wasmSrcPath = join(dir, "static", "wasm");
        const wasmDestPath = wasmDir(config);

        await run("mix", ["deps.get"], { dir });
        await run("mix", ["popcorn.cook"], { dir });

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

const RUNTIME_FILES = [
  "AtomVM.mjs",
  "AtomVM.wasm",
  "popcorn.js",
  "popcorn_iframe.js",
];

export function buildWasm({ dir }) {
  return {
    name: "build-wasm",
    hooks: {
      "astro:config:setup": async ({ logger, config }) => {
        logger.info(`Copying runtime files from '${dir}'...`);
        const srcFiles = await readdir(dir);
        const wasmDestPath = wasmDir(config);

        const cpPromises = srcFiles
          .filter((file) => RUNTIME_FILES.includes(file))
          .map((file) => {
            const src = join(dir, file);
            const dst = join(wasmDestPath, file);
            return cp(src, dst);
          });

        await Promise.all(cpPromises);

        await appendFile(
          join(wasmDestPath, "popcorn.js"),
          "\nwindow.Popcorn = Popcorn;\n",
        );

        logger.info("Wasm files copied to public directory");
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

function run(cmd, args, { dir }) {
  const strCmd = `${cmd} ${args.join(" ")}`;

  return new Promise((resolve, reject) => {
    const child = spawn(cmd, args, {
      cwd: dir,
      stdio: "inherit",
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
