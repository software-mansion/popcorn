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
import buildWasm from "./build-wasm.js";

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
    buildWasm({ dir: "../../examples/iex_wasm" }),
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
