import typescript from "@rollup/plugin-typescript";
import { rollup } from "rollup";
import { wasmModule } from "./wasm-module.js";

export function iframeBundle({ entrypointPath, moduleName }) {
  return {
    name: "iframe-bundle",
    buildStart: async function () {
      const iframeBuild = await rollup({
        input: entrypointPath,
        plugins: [
          typescript(),
          wasmModule({
            wasmPath: "assets/AtomVM.wasm",
            moduleName: "virtual:wasm",
          }),
        ],
      });

      const { output } = await iframeBuild.generate({ format: "iife" });
      const iframeBundleCode = output[0].code;
      this.iframeBundleString = iframeBundleCode;
    },
    resolveId(id) {
      if (id === moduleName) {
        return id;
      }
      return null;
    },
    load(id) {
      if (id === moduleName) {
        const escapedCode = this.iframeBundleString
          .replace(/\\/g, "\\\\")
          .replace(/`/g, "\\`")
          .replace(/\$\{/g, "\\${");
        return `export default \`${escapedCode}\`;`;
      }
    },
  };
}
