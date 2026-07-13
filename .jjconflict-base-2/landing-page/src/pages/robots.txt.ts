import type { APIRoute } from "astro";

const SITE = "https://popcorn.swmansion.com";

export const GET: APIRoute = () =>
  new Response(
    [
      "User-agent: *",
      "Content-Signal: search=yes, ai-input=yes, ai-train=yes",
      "Allow: /",
      "",
      `Sitemap: ${SITE}/sitemap.xml`,
      "",
    ].join("\n"),
    { headers: { "Content-Type": "text/plain; charset=utf-8" } },
  );
