// @ts-check
import { defineConfig } from "astro/config";

import react from "@astrojs/react";
import tailwindcss from "@tailwindcss/vite";
import icon from "astro-icon";
import starlight from "@astrojs/starlight";
// used for build-time generation of diagrams
// import rehypeMermaid from "rehype-mermaid";
import mermaid from "astro-mermaid";
import devtoolsJson from "vite-plugin-devtools-json";
import { buildBundle, buildWasm, cleanWasmDir } from "./build-wasm.js";

// https://astro.build/config
export default defineConfig({
  site: "https://stargazers.club",
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
    starlight({
      title: "Popcorn docs",
      logo: {
        src: "/src/icons/logo-text-adaptive.svg",
        replacesTitle: true,
      },
      customCss: ["/src/styles/docs.css"],
      pagination: false,
      sidebar: [
        {
          label: "Getting started",
          items: ["docs", "installation"],
        },
        {
          label: "Guides",
          items: ["getting-started"],
        },
        {
          label: "Reference",
          items: ["js-api", "elixir-api", "limitations", "architecture"],
        },
      ],
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
