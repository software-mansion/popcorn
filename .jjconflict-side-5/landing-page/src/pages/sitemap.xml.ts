import type { APIRoute } from "astro";

const SITE = "https://popcorn.swmansion.com";
const ROUTES = [
  "/",
  "/demos/eval",
  "/demos/game-of-life",
  "/demos/local-forms",
  "/demos/local-thermostat",
];

export const GET: APIRoute = () => {
  const urls = ROUTES.map(
    (route) =>
      `<url><loc>${SITE}${route}</loc><changefreq>weekly</changefreq></url>`,
  ).join("");

  return new Response(
    `<?xml version="1.0" encoding="UTF-8"?><urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">${urls}</urlset>`,
    { headers: { "Content-Type": "application/xml; charset=utf-8" } },
  );
};
