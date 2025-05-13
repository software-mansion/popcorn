import { Popcorn } from "./wasm/popcorn.js";

async function setup() {
  const popcorn = await Popcorn.init({
    bundlePath: "wasm/app.avm",
    onStdout: (text) => console.trace(text),
    onStderr: (text) => console.warn(text),
  });
  return popcorn;
}

await setup();
