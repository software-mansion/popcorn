import { cp, mkdir, rm } from "node:fs/promises";
import { dirname } from "node:path";
import typescript from "@rollup/plugin-typescript";

function copyFiles(targets) {
  return {
    name: "copy-files",
    async buildEnd() {
      await Promise.all(
        targets.map(async ({ src, dest }) => {
          await mkdir(dirname(dest), { recursive: true });
          await cp(src, dest, { recursive: true });
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

export default [
  {
    input: "src/index.ts",
    output: {
      file: "dist/index.mjs",
      format: "esm",
    },
    cache: false,
    plugins: [
      cleanDir("dist"),
      typescript({ tsconfig: "./tsconfig.json", outputToFilesystem: true }),
    ],
  },
  {
    input: "src/worker.ts",
    output: {
      file: "dist/worker.mjs",
      format: "esm",
    },
    external: ["./beam.mjs"],
    cache: false,
    plugins: [
      typescript({ tsconfig: "./tsconfig.json", outputToFilesystem: true }),
      copyFiles([
        { src: "../out/beam.mjs", dest: "dist/beam.mjs" },
        { src: "../out/beam.emu.mjs", dest: "dist/beam.emu.mjs" },
        { src: "../out/beam.wasm", dest: "dist/beam.wasm" },
        { src: "../out/manifest.json", dest: "dist/otp/manifest.json" },
      ]),
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
      typescript({
        tsconfig: "./plugins/tsconfig.json",
        outputToFilesystem: true,
      }),
      copyFiles([
        {
          src: "../out/runtimes",
          dest: "dist/runtimes",
        },
        {
          src: "plugins/beam_tools/mix.exs",
          dest: "dist/plugins/beam_tools/mix.exs",
        },
        {
          src: "plugins/beam_tools/lib",
          dest: "dist/plugins/beam_tools/lib",
        },
        {
          src: "plugins/beam_tools/patches",
          dest: "dist/plugins/beam_tools/patches",
        },
      ]),
    ],
  },
];
