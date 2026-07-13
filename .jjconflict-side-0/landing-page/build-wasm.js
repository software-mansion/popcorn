import { spawn } from "child_process";
import { cp, rm, mkdir, readdir } from "fs/promises";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

/**
 * @param {{ dir: string, wasmSrcPathDefault?: string, newBundleName: string }} options
 */
export function buildBundle({ dir, wasmSrcPathDefault, newBundleName }) {
  return {
    name: "build-bundle",
    hooks: {
      "astro:config:setup": async ({ logger, config }) => {
        logger.info(`Building bundle in '${dir}'...`);
        const wasmSrcPath = wasmSrcPathDefault ?? join(dir, "dist", "wasm");
        const wasmDestPath = wasmDir(config);

        await run(
          "mise",
          [
            "exec",
            "erlang@26.0.2",
            "elixir@1.17.3-otp-26",
            "--",
            "mix",
            "build",
          ],
          { dir },
        );

        const srcFiles = await readdir(wasmSrcPath);
        const [avm] = srcFiles.filter((path) => path.endsWith(".avm"));
        const src = join(wasmSrcPath, avm);
        const dst = join(wasmDestPath, newBundleName);

        const runtimeSrcPath = join(dir, "priv", "static", "assets", "js");
        const runtimeDestPath = join(fileURLToPath(config.publicDir), "_astro");
        await mkdir(runtimeDestPath, { recursive: true });

        await Promise.all([
          cp(src, dst),
          cp(
            join(runtimeSrcPath, "AtomVM.mjs"),
            join(runtimeDestPath, "AtomVM.mjs"),
          ),
          cp(
            join(runtimeSrcPath, "AtomVM.wasm"),
            join(runtimeDestPath, "AtomVM.wasm"),
          ),
        ]);

        logger.info("Bundle copied");
      },
    },
  };
}

/**
 * @param {{ dir: string, assetsName: string }} options
 */
export function buildOtpAssets({ dir, assetsName }) {
  return {
    name: "build-otp-assets",
    hooks: {
      "astro:config:setup": async ({ logger, config }) => {
        logger.info(`Building OTP assets in '${dir}'...`);
        await run(
          "mise",
          [
            "exec",
            "erlang@28.3.1",
            "elixir@1.19.5-otp-28",
            "--",
            "mix",
            "deps.get",
          ],
          { dir },
        );
        await run(
          "mise",
          [
            "exec",
            "erlang@28.3.1",
            "elixir@1.19.5-otp-28",
            "--",
            "mix",
            "compile",
          ],
          { dir },
        );
        await run("pnpm", ["run", "build"], { dir: join(dir, "assets") });

        const src = join(dir, "dist", "otp");
        const dst = join(fileURLToPath(config.publicDir), "assets", assetsName);
        await rm(dst, { force: true, recursive: true });
        await mkdir(dst, { recursive: true });
        await cp(src, dst, { recursive: true });

        logger.info("OTP assets copied");
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
