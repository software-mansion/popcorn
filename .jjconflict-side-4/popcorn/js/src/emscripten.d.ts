type BeamEmscriptenModule = import("./types").EmscriptenModule;

declare module "*.mjs" {
  const createModule: (
    moduleOverrides?: Partial<BeamEmscriptenModule>,
  ) => Promise<BeamEmscriptenModule>;

  export default createModule;
}
