// @ts-check
import { defineConfig } from "astro/config";

import react from "@astrojs/react";
import tailwindcss from "@tailwindcss/vite";
import icon from "astro-icon";
import devtoolsJson from "vite-plugin-devtools-json";
import { popcorn } from "@swmansion/popcorn/vite";
import {
  buildBundle,
  buildOtpAssets,
  cleanWasmDir,
} from "./build-wasm.js";

// https://astro.build/config
export default defineConfig({
  site: "https://popcorn.swmansion.com",
  redirects: {
    "/docs": "https://hexdocs.pm/popcorn",
  },
  server: {
    headers: {
      "Access-Control-Allow-Origin": "*",
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "credentialless",
      "Cross-Origin-Resource-Policy": "cross-origin",
    },
  },
  vite: {
    plugins: [
      devtoolsJson(),
      tailwindcss(),
      popcorn({
        rootDir: "../examples/iex-wasm",
        app: "iex",
        extraApps: ["logger"],
      }),
    ],
  },
  integrations: [
    react(),
    icon(),
    cleanWasmDir(),
    buildOtpAssets({
      dir: "../examples/game-of-life",
      assetsName: "game-of-life",
    }),
    buildOtpAssets({
      dir: "../examples/eval-in-wasm",
      assetsName: "eval",
    }),
    buildBundle({
      wasmSrcPathDefault:
        "../examples/local-lv-thermostat/priv/static/assets/js/wasm",
      dir: "../examples/local-lv-thermostat",
      newBundleName: "local_thermostat.avm",
    }),
    buildBundle({
      wasmSrcPathDefault:
        "../examples/local-lv-forms/priv/static/assets/js/wasm",
      dir: "../examples/local-lv-forms",
      newBundleName: "local_forms.avm",
    }),
  ],
});
