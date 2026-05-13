import type { APIRoute } from "astro";

export const prerender = true;

const UPSTREAM =
  "https://cdn.cookie-script.com/s/19b5b47f7a8f2606f861864571339358.js";

export const GET: APIRoute = async () => {
  const res = await fetch(UPSTREAM);
  if (!res.ok) {
    throw new Error(
      `Failed to fetch cookie-script from ${UPSTREAM}: ${res.status} ${res.statusText}`,
    );
  }
  const body = await res.text();
  return new Response(body, {
    headers: {
      "Content-Type": "application/javascript; charset=utf-8",
      "Cross-Origin-Resource-Policy": "cross-origin",
    },
  });
};
