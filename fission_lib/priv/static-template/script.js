import { Fission } from "./wasm/fission.js";

async function setup() {
  const fission = await Fission.init({
    bundlePath: "wasm/app.avm",
    onStdout: (text) => console.trace(text),
    onStderr: (text) => console.warn(text),
  });
  return fission;
}

await setup();
