import { Popcorn } from "@swmansion/popcorn";

const popcorn = await Popcorn.init({
  debug: true,
  bundlePath: "/wasm/bundle.avm",
  onStdout: console.log,
  onStderr: console.error,
});
