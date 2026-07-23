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
import { Popcorn } from "@swmansion/popcorn";
import { runCode } from "./eval.js";
import { runCode, errorMessage } from "./eval.js";
import {
  iexCommands,
  decorateIexBlocks,
  addIexClickHandlers,
  startIexSession,
  resetIexSession,
} from "./iex.js";
import { getTerm, openTerminal, writeSystemError } from "./terminal.js";
import { instantiate, TPL_BLOCK, TPL_LAUNCHER, TPL_TOAST } from "./templates.js";

const BUNDLES_SEL = 'meta[name="popcorn-user-bundle"]';
const EVAL_BLOCK_SEL = "pre.popcorn-eval code";

let popcornInstance = null;
let popcornInitPromise = null;

// Live instance shared with every module. Reset swaps it, so consumers read
// it at call time instead of capturing it in closures at bind time.
export function getPopcorn() {
  return popcornInstance;
}
// Bumped per exdoc:loaded so auto block ids never collide across SPA
// navigations — the VM (and its per-block sessions) outlives the page.
let pageEpoch = 0;

let toastEl = null;

function ensureToast() {
  if (toastEl && document.body.contains(toastEl)) return toastEl;

  toastEl = instantiate(TPL_TOAST);
  toastEl
    .querySelector(".popdoc-toast-close")
    .addEventListener("click", hideRuntimeToast);
  document.body.appendChild(toastEl);
  return toastEl;
}

export function showRuntimeToast(state, message) {
  const toast = ensureToast();
  toast.hidden = false;
  toast.classList.toggle("popdoc-toast-error", state === "error");
  toast.querySelector(".popdoc-spinner").hidden = state !== "loading";
  toast.querySelector(".popdoc-toast-icon").hidden = state !== "error";
  toast.querySelector(".popdoc-toast-close").hidden = state !== "error";
  toast.querySelector(".popdoc-toast-message").textContent = message;
}

export function hideRuntimeToast() {
  if (toastEl) toastEl.hidden = true;
}

// The lib transparently reloads its iframe when the VM crashes (heartbeat
// loss, abort): a fresh runtime boots behind the same instance, so all
// session-derived UI state is stale and must be dropped.
function handleRuntimeReload(reason) {
  resetIexSession();
  writeSystemError(
    `popdoc: the runtime restarted (${reason}); session state was lost`,
  );
}

export async function initPopcorn() {
  if (popcornInstance) return popcornInstance;
  if (popcornInitPromise) return popcornInitPromise;

  popcornInitPromise = (async () => {
    const bundlePaths = ["./bundle.avm"];
    try {
      showRuntimeToast("loading", "Loading Popcorn runtime…");
      // TODO: maybe simplify by making `consumer.avm` mandatory
      const userBundles = document.querySelectorAll(BUNDLES_SEL);
      for (const bundleMeta of userBundles) {
        bundlePaths.push(bundleMeta.content);
      }

      popcornInstance = await Popcorn.init({
        debug: true,
        bundlePaths: [...new Set(bundlePaths)],
        onReload: handleRuntimeReload,
      });
      window.popcorn = popcornInstance;
      hideRuntimeToast();
      return popcornInstance;
    } catch (e) {
      popcornInitPromise = null;
      showRuntimeToast("error", `Popcorn failed to load: ${errorMessage(e)}`);
      console.error("Failed to initialize Popcorn runtime:", e);
      throw e;
    }
  })();

  return popcornInitPromise;
}

// Reset tears down the whole runtime on purpose: the terminal and the eval
// blocks share one VM, so bindings, modules, and eval-block sessions all go
// together. (AtomVM has no code server, so this is also the only way to
// unload modules defined in the shell.)
export async function reinitPopcorn() {
  // Invalidate queued and in-flight work BEFORE tearing the runtime down, so
  // their failures die silently instead of rendering into the fresh screen.
  resetIexSession();
  if (popcornInstance) {
    try { popcornInstance.deinit(); } catch (_) {}
    popcornInstance = null;
    popcornInitPromise = null;
  }
  await initPopcorn();
  if (iexCommands.length > 0 || getTerm()) {
    await startIexSession();
  }
}

function decorateBlocks() {
  pageEpoch += 1;
  let blockIndex = 0;
  const blocks = [];

  for (const codeEl of document.querySelectorAll(EVAL_BLOCK_SEL)) {
    const preEl = codeEl.parentElement;
    if (preEl === null || preEl.dataset.popdocProcessed === "true") continue;

    preEl.dataset.popdocProcessed = "true";
    const blockId =
      preEl.id.length > 0
        ? preEl.id
        : `popdoc-eval-${pageEpoch}-${++blockIndex}`;

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
    block.button.addEventListener("click", () => runCode(block));
  }
}

let launcherEl = null;

// Small fixed "iex" pill in the bottom-right corner, so the terminal is
// reachable from any page — not only through an iex> prompt in a block.
function ensureIexLauncher() {
  if (launcherEl && document.body.contains(launcherEl)) return launcherEl;

  launcherEl = instantiate(TPL_LAUNCHER);
  launcherEl.addEventListener("click", async () => {
    launcherEl.disabled = true;
    launcherEl.classList.add("popdoc-iex-launcher--loading");
    try {
      await initPopcorn();
      await startIexSession();
      openTerminal();
    } catch (error) {
      // initPopcorn/startIexSession already reported through the toast.
      console.error("popdoc: failed to open the IEx terminal:", error);
    } finally {
      launcherEl.disabled = false;
      launcherEl.classList.remove("popdoc-iex-launcher--loading");
    }
  });

  document.body.appendChild(launcherEl);
  return launcherEl;
}

window.addEventListener("exdoc:loaded", async () => {
  const blocks = decorateBlocks();
  decorateIexBlocks();
  addClickHandlers(blocks);
  ensureIexLauncher();
  addIexClickHandlers();

  if (!popcornInstance) return;

  // Eval-block sessions from the previous page are unreachable now; drop
  // them before any new Run can start (the GenServer handles calls in
  // order, so this cannot outrun a fresh parse_elixir).
  popcornInstance
    .call(["clear_sessions"])
    .catch((error) =>
      console.error("popdoc: failed to clear stale sessions:", error),
    );

  if (iexCommands.length > 0) {
    try {
      await startIexSession();
    } catch (error) {
      console.error("popdoc: failed to start the IEx session:", error);
    }
  }
});
