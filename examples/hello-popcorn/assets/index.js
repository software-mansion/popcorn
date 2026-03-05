import { Popcorn } from "@swmansion/popcorn";

await Popcorn.init({ bundlePath: "/wasm/bundle.avm", onStdout: console.log });
