import { Popcorn } from "@swmansion/popcorn";

const BUNDLES_SEL = 'meta[name="popcorn-user-bundle"]';
const EVAL_BLOCK_SEL = "pre.popcorn-eval code";
const PENDING_DELAY_MS = 200;

function assert(condition) {
  if (!condition) {
    throw new Error("Assertion failed");
  }
}

async function runCode(popcorn, { code, blockId, button, output }) {
  button.disabled = true;
  output.classList.remove("popdoc-output-error");

  const stopLogCapture = startLogCapture(popcorn);
  const pendingTimer = setTimeout(() => {
    output.hidden = false;
    output.textContent = "Evaluating...";
    output.classList.add("popdoc-output-pending");
  }, PENDING_DELAY_MS);

  let result;
  let logs;

  try {
    result = await popcorn.call(["eval_elixir", code, blockId], {
      timeoutMs: 30_000,
    });
  } catch (error) {
    assert(error instanceof Error);
    result = { ok: false, error };
  } finally {
    logs = stopLogCapture();
    clearTimeout(pendingTimer);
    output.classList.remove("popdoc-output-pending");
    button.disabled = false;
  }

  renderOutput(output, { result, logs });
}

function decorateBlocks() {
  let blockIndex = 0;
  const blocks = [];

  for (const codeEl of document.querySelectorAll(EVAL_BLOCK_SEL)) {
    const preEl = codeEl.parentElement;
    if (!preEl || preEl.dataset.popdocProcessed) continue;

    preEl.dataset.popdocProcessed = "true";
    const blockId = preEl.id || `popdoc-eval-${++blockIndex}`;

    const wrapper = document.createElement("div");
    wrapper.className = "popdoc-block";
    preEl.insertAdjacentElement("afterend", wrapper);
    wrapper.appendChild(preEl);

    const button = document.createElement("button");
    button.className = "popdoc-run";
    button.type = "button";
    button.textContent = "Run";
    button.disabled = true;
    wrapper.insertBefore(button, preEl);

    const output = document.createElement("pre");
    output.className = "popdoc-output";
    output.hidden = true;
    wrapper.appendChild(output);

    blocks.push({
      code: codeEl.textContent,
      blockId,
      button,
      output,
    });
  }

  return blocks;
}

function addClickHandlers(popcorn, blocks) {
  for (const block of blocks) {
    block.button.disabled = false;
    block.button.addEventListener("click", () => runCode(popcorn, block));
  }
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

function startLogCapture(popcorn) {
  const stdout = [];
  const stderr = [];

  const stdoutListener = (message) => stdout.push(message);
  const stderrListener = (message) => stderr.push(message);

  popcorn.registerLogListener(stdoutListener, "stdout");
  popcorn.registerLogListener(stderrListener, "stderr");

  return () => {
    popcorn.unregisterLogListener(stdoutListener, "stdout");
    popcorn.unregisterLogListener(stderrListener, "stderr");
    return { stdout, stderr };
  };
}

function renderOutput(output, { result, logs }) {
  const lines = [];

  if (logs.stdout.length > 0) {
    lines.push(...logs.stdout);
  }

  if (logs.stderr.length > 0) {
    lines.push(...logs.stderr);
  }

  if (result.ok) {
    assert(typeof result.data === "string");

    lines.push(result.data);
    output.classList.remove("popdoc-output-error");
  } else {
    assert(result.error instanceof Error);

    lines.push(result.error.message);
    output.classList.add("popdoc-output-error");
  }

  output.hidden = false;
  output.textContent = lines.join("\n");
}

window.addEventListener("exdoc:loaded", async () => {
  const blocks = decorateBlocks();
  const popcorn = await initPopcorn();
  window.popcorn = popcorn;
  addClickHandlers(popcorn, blocks);
});
