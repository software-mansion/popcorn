import { Popcorn } from "./vendor.js";

await Popcorn.init({
  onStdout: console.log,
  debug: true,
  bundlePath: "bundle.avm",
});
