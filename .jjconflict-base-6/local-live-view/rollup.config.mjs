import resolve from "@rollup/plugin-node-resolve";
import typescript from "@rollup/plugin-typescript";
import { popcorn } from "@swmansion/popcorn/rollup";
import { dts } from "rollup-plugin-dts";

export default [
  {
    input: "assets/local_live_view/index.ts",
    output: {
      file: "priv/static/local_live_view.js",
      format: "esm",
    },
    plugins: [
      resolve(),
      typescript({ tsconfig: "./tsconfig.json" }),
      // Copies iframe.mjs, AtomVM.mjs, AtomVM.wasm into priv/static/.
      // bundlePaths is empty — .avm files are loaded at runtime via Popcorn.init().
      popcorn({ bundlePaths: [] }),
    ],
  },
  {
    input: "assets/local_live_view/index.ts",
    output: {
      file: "priv/static/local_live_view.d.ts",
      format: "esm",
    },
    plugins: [dts()],
  },
];
