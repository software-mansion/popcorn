import { Popcorn } from "@swmansion/popcorn";

await Popcorn.init({
  bundlePaths: ["/wasm/bundle.avm"],
  onStdout: console.log,
});
