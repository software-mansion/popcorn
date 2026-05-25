import { Popcorn } from "@swmansion/popcorn";

const BUNDLES_SEL = 'meta[name="popcorn-user-bundle"]';
const EVAL_BLOCK_SEL = "pre.popcorn-eval code";
const PENDING_DELAY_MS = 200;
const EVAL_TIMEOUT_MS = 30_000;

function assert(condition) {
  if (!condition) {
    throw new Error("Assertion failed");
  }
}

async function runCode(popcorn, { code, blockId, button, output }) {
  button.disabled = true;
  output.classList.remove("popdoc-output-error");
  output.classList.remove("popdoc-output-pending");
  output.hidden = true;
  output.textContent = "";

  const pendingTimer = setTimeout(() => {
    output.hidden = false;
    output.textContent = "Evaluating...";
    output.classList.add("popdoc-output-pending");
  }, PENDING_DELAY_MS);

  const entries = [];
  let error = null;
  const deadline = performance.now() + EVAL_TIMEOUT_MS;
  const remaining = () => Math.max(0, deadline - performance.now());

  const parsed = await popcorn.call(["parse_elixir", code, blockId], {
    timeoutMs: remaining(),
  });

  if (parsed.ok) {
    const { expressions } = parsed.data;
    for (let i = 0; i < expressions.length; i++) {
      const budget = remaining();
      if (budget === 0) {
        error = `Evaluation timed out after ${EVAL_TIMEOUT_MS}ms`;
        break;
      }

      const stopLogCapture = startLogCapture(popcorn);
      const step = await popcorn.call(["eval_one", blockId, i], { timeoutMs: budget });
      const logs = stopLogCapture();

      if (!step.ok) {
        error = step.error.message;
        break;
      }

      entries.push({ ...step.data, logs });
      if (step.data.error) {
        error = step.data.error;
        break;
      }
    }
  } else {
    error = parsed.error.message;
  }

  clearTimeout(pendingTimer);
  output.classList.remove("popdoc-output-pending");
  button.disabled = false;
  renderOutput(output, { entries, error });
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

function renderOutput(output, { entries, error }) {
  const lines = entries.flatMap((e) => [...e.logs.stdout, ...e.logs.stderr]);
  const last = entries[entries.length - 1];

  if (error) {
    lines.push(error);
  } else if (last) {
    lines.push(last.result);
  }

  output.classList.toggle("popdoc-output-error", Boolean(error));
  output.hidden = lines.length === 0;
  output.textContent = lines.join("\n");
}

window.addEventListener("exdoc:loaded", async () => {
  const blocks = decorateBlocks();
  const popcorn = await initPopcorn();
  window.popcorn = popcorn;
  addClickHandlers(popcorn, blocks);
});
