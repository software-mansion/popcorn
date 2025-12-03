import typescript from "@rollup/plugin-typescript";
import { iframeBundle } from "./plugins/iframe-bundle.js";
import { wasmModule } from "./plugins/wasm-module.js";

/**
 * Right now, we
 */
const config = [
  {
    input: "src/index.ts",
    output: {
      file: "dist/index.js",
      format: "es",
    },

    plugins: [
      iframeBundle({
        entrypointPath: "src/iframe.ts",
        moduleName: "virtual:iframe-bundle",
      }),
      wasmModule({
        wasmPath: "assets/AtomVM.wasm",
        moduleName: "virtual:wasm",
      }),
      typescript(),
    ],
  },
];

export default config;
