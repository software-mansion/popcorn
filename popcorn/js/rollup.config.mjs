import { mkdir, rm, symlink } from "node:fs/promises";
import typescript from "@rollup/plugin-typescript";

export default [
  {
    input: "src/index.ts",
    output: {
      file: "../out/js/index.mjs",
      format: "esm",
    },
    cache: false,
    plugins: [
      {
        name: "shared-output",
        async buildStart() {
          await rm("../out/js", { recursive: true, force: true });
          await mkdir("../out/js", { recursive: true });
          await symlink("../runtimes", "../out/js/runtimes", "dir");
          await rm("dist", { recursive: true, force: true });
          await symlink("../out/js", "dist", "dir");
          await mkdir("../elixir/priv", { recursive: true });
          await rm("../elixir/priv/static", { recursive: true, force: true });
          await symlink("../../out/js", "../elixir/priv/static", "dir");
        },
      },
      typescript({ tsconfig: "./tsconfig.json", outputToFilesystem: true }),
    ],
  },
  {
    input: "src/worker.ts",
    output: {
      file: "../out/js/worker.mjs",
      format: "esm",
    },
    external: ["./beam.mjs"],
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
      dir: "../out/js/plugins",
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
    ],
  },
];
