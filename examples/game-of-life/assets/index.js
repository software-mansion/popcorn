import { Popcorn } from "@swmansion/popcorn";

const popcorn = await Popcorn.init({
  debug: true,
  bundlePath: "/wasm/bundle.avm",
  waitForDefaultReceiver: false,
  onStdout: console.log,
  onStderr: console.error,
});
