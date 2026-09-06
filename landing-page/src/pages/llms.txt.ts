import type { APIRoute } from "astro";

const SITE = "https://popcorn.swmansion.com";

const SUMMARY =
  "Popcorn compiles Elixir to WebAssembly and runs it in the browser with no extra setup. Built by Software Mansion.";

const PAGES = [
  {
    path: "/",
    title: "Popcorn",
    description: "Popcorn landing page",
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
