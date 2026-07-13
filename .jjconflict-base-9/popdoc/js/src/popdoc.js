import { Popcorn } from "@swmansion/popcorn";
import { runCode } from "./eval.js";
import { instantiate, TPL_BLOCK } from "./templates.js";

const BUNDLES_SEL = 'meta[name="popcorn-user-bundle"]';
const EVAL_BLOCK_SEL = "pre.popcorn-eval code";

let popcornInstance = null;

export function getPopcorn() {
  return popcornInstance;
}

async function initPopcorn() {
  const bundlePaths = ["./bundle.avm"];
  try {
    // TODO: maybe simplify by making `consumer.avm` mandatory
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

function decorateBlocks() {
  let blockIndex = 0;
  const blocks = [];

  for (const codeEl of document.querySelectorAll(EVAL_BLOCK_SEL)) {
    const preEl = codeEl.parentElement;
    if (preEl === null || preEl.dataset.popdocProcessed === "true") continue;

    preEl.dataset.popdocProcessed = "true";
    const blockId =
      preEl.id.length > 0 ? preEl.id : `popdoc-eval-${++blockIndex}`;

    const wrapper = instantiate(TPL_BLOCK);
    preEl.insertAdjacentElement("afterend", wrapper);
    wrapper.insertBefore(preEl, wrapper.querySelector(".popdoc-output"));

    blocks.push({
      code: codeEl.textContent,
      blockId,
      button: wrapper.querySelector(".popdoc-run"),
      output: wrapper.querySelector(".popdoc-output"),
      status: wrapper.querySelector(".popdoc-status"),
    });
  }

  return blocks;
}

function addClickHandlers(blocks) {
  for (const block of blocks) {
    block.button.disabled = false;
    block.button.addEventListener("click", () => runCode(block));
  }
}

window.addEventListener("exdoc:loaded", async () => {
  const blocks = decorateBlocks();
  popcornInstance = await initPopcorn();
  window.popcorn = popcornInstance;
  addClickHandlers(blocks);
});
