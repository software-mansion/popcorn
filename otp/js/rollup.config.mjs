import { copyFile, mkdir } from "node:fs/promises";
import { dirname } from "node:path";
import typescript from "@rollup/plugin-typescript";

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

export default [
  // Main library
  {
    input: {
      index: "src/index.ts",
      worker: "src/worker.ts",
    },
    output: {
      dir: "dist",
      format: "esm",
      entryFileNames: "[name].mjs",
      preserveModules: true,
      preserveModulesRoot: "src",
    },
    cache: false,
    external: (id) => id.startsWith("node:"),
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
    },
    external: (id) =>
      id.startsWith("node") || ["esbuild", "rollup", "vite"].includes(id),
    cache: false,
    plugins: [
      typescript({
        tsconfig: "./plugins/tsconfig.json",
        outputToFilesystem: true,
      }),
      copyFiles([
        { src: "plugins/tarballs.exs", dest: "dist/plugins/tarballs.exs" },
      ]),
    ],
  },
];
