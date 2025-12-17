// @ts-check
import { defineConfig } from "astro/config";

import react from "@astrojs/react";
import tailwindcss from "@tailwindcss/vite";
import icon from "astro-icon";
// used for build-time generation of diagrams
// import rehypeMermaid from "rehype-mermaid";
import mermaid from "astro-mermaid";
import devtoolsJson from "vite-plugin-devtools-json";
import { buildBundle, buildWasm, cleanWasmDir } from "./build-wasm.js";

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
    plugins: [devtoolsJson(), tailwindcss()],
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
    buildBundle({ dir: "../../examples/iex_wasm", newBundleName: "iex.avm" }),
    buildBundle({
      dir: "../../examples/game_of_life",
      newBundleName: "gol.avm",
    }),
    buildBundle({
      dir: "../../examples/eval_in_wasm",
      newBundleName: "eval.avm",
    }),
    buildBundle({
      wasmSrcPathDefault: "../../examples/local_thermostat/static/local_live_view/wasm",
      dir: "../../examples/local_thermostat",
      newBundleName: "local_thermostat.avm",
    }),
    buildBundle({
      wasmSrcPathDefault: "../../examples/form_demo/priv/static/local_live_view/wasm",
      dir: "../../examples/form_demo/local",
      newBundleName: "local_forms.avm",
    }),
    // take runtime from iex wasm
    buildWasm({ dir: "../../examples/iex_wasm/static/wasm" }),
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
