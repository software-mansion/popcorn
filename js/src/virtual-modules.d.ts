declare module "virtual:iframe-bundle" {
  const content: string;
  export default content;
}

declare module "virtual:wasm" {
  const wasm: Uint8Array<ArrayBuffer>;
  const wasmPath: string;
}
