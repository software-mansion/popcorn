import { Popcorn } from "@swmansion/popcorn";

const BUNDLES_SEL = 'meta[name="popcorn-user-bundle"]';
async function initPopcorn() {
  const bundlePaths = ["./bundle.avm"];
  try {
    const userBundles = document.querySelectorAll(BUNDLES_SEL);
    for (const bundleMeta of userBundles) {
      bundlePaths.push(bundleMeta.content);
    }

    return Popcorn.init({
      debug: true,
      bundlePaths: [...new Set(bundlePaths)],
    });
  } catch (e) {
    console.error("Failed to initialize Popcorn runtime:", e);
    throw e;
  }
}

window.addEventListener("exdoc:loaded", async () => {
  const popcorn = await initPopcorn();
  window.popcorn = popcorn;
});
