import { copyFile, mkdir } from "fs/promises";
import { resolve } from "path";
import typescript from "@rollup/plugin-typescript";

// Resolves ./AtomVM.mjs imports to assets/AtomVM.mjs
function resolveAtomVM() {
  return {
    name: "resolve-atomvm",
    resolveId(source) {
      if (source === "./AtomVM.mjs") {
        return resolve("assets/AtomVM.mjs");
      }
      return null;
    },
  };
}

function copyAssets(targets) {
  return {
    name: "copy-assets",
    async buildEnd() {
      await Promise.all(
        targets.map(async ({ src, dest }) => {
          await mkdir(dest, { recursive: true });
          await copyFile(src, `${dest}/${src.split("/").pop()}`);
        }),
      );
    },
  };
}

export default [
  // Main library
  {
    input: {
      iframe: "src/iframe.ts",
      AtomVM: "assets/AtomVM.mjs",
      index: "src/index.ts",
    },
    output: {
      dir: "dist",
      format: "esm",
      entryFileNames: "[name].mjs",
      preserveModules: true,
      preserveModulesRoot: "src",
    },
    cache: false,
    // Suppress eval warning for AtomVM.mjs (emscripten-generated code)
    onwarn(warning, warn) {
      if (warning.code === "EVAL" && warning.id?.includes("AtomVM.mjs")) {
        return;
      }
      warn(warning);
    },
    plugins: [
      resolveAtomVM(),
      typescript({ tsconfig: "./src/tsconfig.json", outputToFilesystem: true }),
      copyAssets([{ src: "assets/AtomVM.wasm", dest: "dist" }]),
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
    external: [
      "vite",
      "rollup",
      "esbuild",
      "http",
      "fs",
      "fs/promises",
      "path",
      "url",
    ],
    cache: false,
    plugins: [
      typescript({
        tsconfig: "./plugins/tsconfig.json",
        outputToFilesystem: true,
      }),
    ],
  },
];
