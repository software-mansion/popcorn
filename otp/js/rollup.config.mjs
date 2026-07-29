import { execFile } from "node:child_process";
import { copyFile, cp, mkdir, readdir, rm } from "node:fs/promises";
import { dirname, join } from "node:path";
import { promisify } from "node:util";
import typescript from "@rollup/plugin-typescript";

const execFileAsync = promisify(execFile);

function copyFiles(targets) {
  return {
    name: "copy-files",
    async buildEnd() {
      await Promise.all(
        targets.map(async ({ src, dest }) => {
          await mkdir(dirname(dest), { recursive: true });
          await copyFile(src, dest);
        }),
      );
    },
  };
}

function cleanDir(dir) {
  return {
    name: "clean-dir",
    async buildStart() {
      await rm(dir, { recursive: true, force: true });
    },
  };
}

function cleanModules(dir) {
  return {
    name: "clean-modules",
    async buildStart() {
      await mkdir(dir, { recursive: true });
      const entries = await readdir(dir, { withFileTypes: true });
      await Promise.all(
        entries
          .filter((entry) => entry.isFile() && entry.name.endsWith(".mjs"))
          .map((entry) => rm(join(dir, entry.name))),
      );
    },
  };
}

function buildTreeshake() {
  return {
    name: "build-treeshake",
    async buildEnd() {
      const cwd = "plugins/treeshake";
      await execFileAsync("mix", ["compile"], {
        cwd,
        env: { ...process.env, MIX_ENV: "prod" },
      });
      await cp(
        join(cwd, "_build/prod/lib/popcorn_treeshake/ebin"),
        "dist/plugins/treeshake/ebin",
        { recursive: true },
      );
    },
  };
}

export default [
  {
    input: "src/index.ts",
    output: {
      file: "dist/index.mjs",
      format: "esm",
    },
    cache: false,
    plugins: [
      cleanModules("dist"),
      typescript({ tsconfig: "./tsconfig.json", outputToFilesystem: true }),
    ],
  },
  {
    input: "src/worker.ts",
    output: {
      file: "dist/worker.mjs",
      format: "esm",
      paths: (id) =>
        id.endsWith("/assets/beam.mjs") ? "./assets/beam.mjs" : id,
    },
    external: (id) => id.endsWith("/assets/beam.mjs"),
    cache: false,
    plugins: [
      typescript({ tsconfig: "./tsconfig.json", outputToFilesystem: true }),
    ],
  },
  {
    input: {
      vite: "plugins/vite.ts",
      rollup: "plugins/rollup.ts",
      esbuild: "plugins/esbuild.ts",
    },
    output: {
      dir: "dist/plugins",
      format: "esm",
      entryFileNames: "[name].mjs",
      chunkFileNames: "[name].mjs",
    },
    external: (id) =>
      id.startsWith("node:") || ["esbuild", "rollup", "vite"].includes(id),
    cache: false,
    plugins: [
      cleanDir("dist/plugins"),
      typescript({
        tsconfig: "./plugins/tsconfig.json",
        outputToFilesystem: true,
      }),
      buildTreeshake(),
      copyFiles([
        { src: "plugins/tarballs.exs", dest: "dist/plugins/tarballs.exs" },
      ]),
    ],
  },
];
