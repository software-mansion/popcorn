import { Popcorn } from "@swmansion/popcorn";

const bundlePath =
  new URLSearchParams(window.location.search).get("bundlePath") ?? undefined;

window.popcorn_promise = Popcorn.init({
  bundlePath,
  onStdout: (output) => console.log(`[popcorn stdout] ${output}`),
});
