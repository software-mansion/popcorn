type BeamEmscriptenModule = import("./types").EmscriptenModule;

declare module "*.mjs" {
  const createModule: (
    moduleOverrides?: Partial<BeamEmscriptenModule>,
  ) => Promise<BeamEmscriptenModule>;

  export const wasmUrl: string;
  export default createModule;
}
