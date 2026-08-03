import { AtomVM } from "@swmansion/popcorn";

await AtomVM.init({
  bundlePaths: ["/wasm/bundle.avm"],
  onStdout: console.log,
});
