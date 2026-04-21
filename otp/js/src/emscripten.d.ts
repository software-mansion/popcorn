type BeamEmscriptenModule = import("./types").EmscriptenModule;

declare module "*.emu" {
  const createModule: (
    moduleOverrides?: Partial<BeamEmscriptenModule>,
  ) => Promise<BeamEmscriptenModule>;

  export default createModule;
}
