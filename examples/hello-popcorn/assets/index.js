import { Popcorn } from "@swmansion/popcorn";

await Popcorn.init({
  bundlePath: "/wasm/bundle.avm",
  waitForDefaultReceiver: false,
  onStdout: console.log,
});
