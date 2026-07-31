import type { APIRoute } from "astro";

const SITE = "https://popcorn.swmansion.com";

const SUMMARY =
  "Popcorn compiles Elixir to WebAssembly and runs it in the browser with no extra setup. Built by Software Mansion.";

const PAGES = [
  {
    path: "/",
    title: "Popcorn",
    description: "What Popcorn does and how to start",
  },
  {
    path: "/demos/eval",
    title: "Demo: eval",
    description: "Evaluate Elixir expressions in the browser",
  },
  {
    path: "/demos/game-of-life",
    title: "Demo: game of life",
    description: "Conway's game of life running on Elixir in Wasm",
  },
  {
    path: "/demos/local-forms",
    title: "Demo: local forms",
    description: "Form handling driven entirely client-side",
  },
  {
    path: "/demos/local-thermostat",
    title: "Demo: local thermostat",
    description: "Stateful UI backed by an Elixir process in the browser",
  },
];

export const GET: APIRoute = () =>
  new Response(
    [
      "# Popcorn",
      "",
      `> ${SUMMARY}`,
      "",
      "## Pages",
      "",
      ...PAGES.map(
        (page) => `- [${page.title}](${SITE}${page.path}): ${page.description}`,
      ),
      "",
      "## Documentation",
      "",
      "- [Popcorn on HexDocs](https://hexdocs.pm/popcorn): api reference and guides",
      "",
    ].join("\n"),
    { headers: { "Content-Type": "text/plain; charset=utf-8" } },
  );
