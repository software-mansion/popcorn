import { Popcorn } from "./wasm/popcorn.js";

async function setup() {
  const popcorn = await Popcorn.init({
    onStdout: (text) => console.trace(text),
    onStderr: (text) => console.warn(text),
  });
  return popcorn;
}

await setup();
