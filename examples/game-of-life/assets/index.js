import { Popcorn } from "@swmansion/popcorn";

const popcorn = await Popcorn.init({
  debug: true,
  bundlePaths: ["/wasm/bundle.avm"],
  onStdout: console.log,
  onStderr: console.error,
});
