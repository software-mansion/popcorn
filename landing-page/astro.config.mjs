// @ts-check
import { defineConfig } from "astro/config";

import react from "@astrojs/react";
import tailwindcss from "@tailwindcss/vite";
import icon from "astro-icon";
// used for build-time generation of diagrams
// import rehypeMermaid from "rehype-mermaid";
import mermaid from "astro-mermaid";
import devtoolsJson from "vite-plugin-devtools-json";
import { popcorn } from "@swmansion/popcorn/vite";
import { buildBundle, cleanWasmDir } from "./build-wasm.js";

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
    plugins: [devtoolsJson(), tailwindcss(), popcorn({ bundlePaths: [] })],
  },
  integrations: [
    react(),
    icon(),
    mermaid({
      autoTheme: true,
      mermaidConfig: {
        sequence: {
          mirrorActors: false,
        },
      },
    }),
    cleanWasmDir(),
    buildBundle({ dir: "../examples/iex-wasm", newBundleName: "iex.avm" }),
    buildBundle({
      dir: "../examples/game-of-life",
      newBundleName: "gol.avm",
    }),
    buildBundle({
      dir: "../examples/eval-in-wasm",
      newBundleName: "eval.avm",
    }),
    buildBundle({
      wasmSrcPathDefault:
        "../examples/local-lv-thermostat/priv/static/assets/js/wasm",
      dir: "../examples/local-lv-thermostat",
      newBundleName: "local_thermostat.avm",
    }),
    buildBundle({
      wasmSrcPathDefault:
        "../examples/local-lv-forms/priv/static/local_live_view/wasm",
      dir: "../examples/local-lv-forms/local",
      newBundleName: "local_forms.avm",
    }),
  ],
  markdown: {
    // used for build-time generation of diagrams
    // rehypePlugins: [
    //   [
    //     rehypeMermaid,
    //     {
    //       strategy: "img-svg",
    //       dark: true,
    //       mermaidConfig: {
    //         sequence: {
    //           mirrorActors: false,
    //         },
    //       },
    //     },
    //   ],
    // ],
    // syntaxHighlight: {
    //   excludeLangs: ["mermaid", "math"],
    // },
  },
});
