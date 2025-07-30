export async function onPageLoad(fn: () => Promise<void>) {
  document.addEventListener("astro:page-load", fn);
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", fn);
  } else {
    await fn();
  }
}
