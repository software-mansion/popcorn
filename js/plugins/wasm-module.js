import { readFile } from "fs/promises";

export function wasmModule({ wasmPath, moduleName }) {
  return {
    name: "wasm-module",
    buildStart: async function () {
      const wasmBuffer = await readFile(wasmPath);
      this.wasmBuffer = wasmBuffer;
      this.wasmPath = wasmPath;
    },
    resolveId(id) {
      if (id === moduleName) {
        return id;
      }
      return null;
    },
    load(id) {
      if (id === moduleName) {
        const hexString = this.wasmBuffer.toString("hex");

        return `
const wasm = new Uint8Array(${this.wasmBuffer.length});
wasm.setFromHex("${hexString}");
const wasmBlob = new Blob([wasm], { type: 'application/wasm' });
export { wasm };
export const wasmPath = URL.createObjectURL(wasmBlob);
        `.trim();
      }
    },
  };
}
