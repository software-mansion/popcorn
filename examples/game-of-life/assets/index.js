import { AtomVM } from "@swmansion/popcorn";

const popcorn = await AtomVM.init({
  debug: true,
  bundlePaths: ["/wasm/bundle.avm"],
  onStdout: console.log,
  onStderr: console.error,
});
