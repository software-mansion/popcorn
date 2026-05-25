import { Popcorn } from "@swmansion/popcorn";

const BUNDLES_SEL = 'meta[name="popcorn-user-bundle"]';
const EVAL_BLOCK_SEL = "pre.popcorn-eval code";
const PENDING_DELAY_MS = 200;
const EVAL_TIMEOUT_MS = 30_000;

const tpl = (html) => {
  const t = document.createElement("template");
  t.innerHTML = html.trim();
  return t;
};

const TPL_BLOCK = tpl(`
  <div class="popdoc-block">
    <div class="popdoc-header">
      <button class="popdoc-run" type="button" disabled>Run</button>
      <span class="popdoc-status" hidden></span>
    </div>
    <div class="popdoc-output" hidden></div>
  </div>
`);

const TPL_STATUS = tpl(`
  <span>
    <span class="popdoc-dot"></span>
    <span></span>
  </span>
`);

const TPL_SKELETON = tpl(`
  <div class="popdoc-result"><span class="popdoc-empty">(empty)</span></div>
`);

const TPL_TOGGLE = tpl(`
  <button type="button" class="popdoc-toggle">
    <span class="popdoc-chev">▸</span>
    <span>show all results</span>
  </button>
`);

const TPL_TOP = tpl(`
  <div class="popdoc-top">
    <div class="popdoc-result"></div>
  </div>
`);

const TPL_STDIO = tpl(`
  <div class="popdoc-stdio">
    <span class="popdoc-label"></span>
    <span></span>
  </div>
`);

const TPL_ROW = tpl(`
  <div class="popdoc-row">
    <span class="popdoc-cell-snippet"></span>
    <span class="popdoc-cell-result"></span>
  </div>
`);

const TPL_ERROR_TOP = tpl(`
  <div class="popdoc-top-error">
    <span class="popdoc-cell-snippet"></span>
    <span class="popdoc-cell-result popdoc-result-err"></span>
  </div>
`);

function instantiate(template) {
  return template.content.firstElementChild.cloneNode(true);
}

function hasMultipleExpressions(code) {
  return code.trim().split(/\n+/).filter((l) => l.trim().length > 0).length > 1;
}

function formatError(error) {
  if (error === null) return "";
  return error.type !== null ? `${error.type}: ${error.message}` : error.message;
}

async function runCode(popcorn, block) {
  const { code, blockId, button, output, status } = block;

  button.disabled = true;
  output.replaceChildren();
  output.classList.remove("popdoc-output-pending", "popdoc-expanded");
  output.classList.toggle("popdoc-output-multi", hasMultipleExpressions(code));
  output.hidden = true;
  status.hidden = true;
  status.replaceChildren();

  const pendingTimer = setTimeout(() => {
    output.hidden = false;
    output.classList.add("popdoc-output-pending");
    output.replaceChildren(instantiate(TPL_SKELETON));
    if (hasMultipleExpressions(code)) {
      output.appendChild(instantiate(TPL_TOGGLE));
    }
  }, PENDING_DELAY_MS);

  const deadline = performance.now() + EVAL_TIMEOUT_MS;
  const remaining = () => Math.max(0, deadline - performance.now());
  const startedAt = performance.now();

  const parsed = await popcorn.call(["parse_elixir", code, blockId], { timeoutMs: remaining() });

  clearTimeout(pendingTimer);
  output.classList.remove("popdoc-output-pending");

  if (!parsed.ok) {
    const parseError = { kind: "error", type: null, message: parsed.error.message, stacktrace: "" };
    output.hidden = false;
    output.replaceChildren();
    const top = instantiate(TPL_TOP);
    const resultRow = top.querySelector(".popdoc-result");
    resultRow.classList.add("popdoc-result-err");
    resultRow.textContent = formatError(parseError);
    output.appendChild(top);
    renderStatus(status, { error: parseError, elapsedMs: Math.round(performance.now() - startedAt) });
    button.disabled = false;
    return;
  }

  const expressions = parsed.data.expressions;
  output.hidden = false;
  const scaffold = buildScaffold(output, expressions);

  const entries = [];
  let error = null;

  for (let i = 0; i < expressions.length; i++) {
    const budget = remaining();
    if (budget === 0) {
      error = { kind: "error", type: null, message: `Evaluation timed out after ${EVAL_TIMEOUT_MS}ms`, stacktrace: "" };
      scaffold.markError(i, expressions[i], error);
      break;
    }

    const stopLogCapture = startLogCapture(popcorn);
    const step = await popcorn.call(["eval_one", blockId, i], { timeoutMs: budget });
    const logs = stopLogCapture();

    if (!step.ok) {
      error = { kind: "error", type: null, message: step.error.message, stacktrace: "" };
      scaffold.markError(i, expressions[i], error);
      break;
    }

    const stepError = step.data.error ?? null;
    entries.push({ ...step.data, error: stepError, logs });

    if (stepError !== null) {
      error = stepError;
      scaffold.markError(i, expressions[i], stepError);
      break;
    }

    scaffold.markSuccess(i, step.data.result);
  }

  if (error === null && entries.length > 0) {
    scaffold.finalize(entries[entries.length - 1].result);
  }
  scaffold.appendStdio(entries);
  renderStatus(status, { error, elapsedMs: Math.round(performance.now() - startedAt) });
  button.disabled = false;
}

function buildScaffold(output, expressions) {
  output.replaceChildren();

  const top = instantiate(TPL_TOP);
  let topResult = top.querySelector(".popdoc-result");
  const emptySpan = document.createElement("span");
  emptySpan.className = "popdoc-empty";
  emptySpan.textContent = "(empty)";
  topResult.appendChild(emptySpan);
  output.appendChild(top);

  const rowCells = [];
  const hasList = expressions.length > 1;

  if (hasList) {
    const toggle = instantiate(TPL_TOGGLE);
    const chev = toggle.querySelector(".popdoc-chev");
    const label = toggle.lastElementChild;
    toggle.addEventListener("click", () => {
      const expanded = output.classList.toggle("popdoc-expanded");
      chev.textContent = expanded ? "▾" : "▸";
      label.textContent = expanded ? "hide all results" : "show all results";
    });
    output.appendChild(toggle);

    const list = document.createElement("div");
    list.className = "popdoc-list";
    for (const expr of expressions) {
      const row = instantiate(TPL_ROW);
      row.querySelector(".popdoc-cell-snippet").textContent = expr.snippet;
      const cell = row.querySelector(".popdoc-cell-result");
      cell.textContent = "—";
      row.classList.add("popdoc-row-skipped");
      rowCells.push({ row, cell });
      list.appendChild(row);
    }
    output.appendChild(list);
  }

  return {
    markSuccess(i, result) {
      if (hasList) {
        const { row, cell } = rowCells[i];
        row.classList.remove("popdoc-row-skipped");
        cell.classList.remove("popdoc-result-err");
        cell.textContent = result;
      }
    },
    finalize(lastResult) {
      topResult.textContent = lastResult;
    },
    markError(i, expr, error) {
      if (hasList) {
        const { row, cell } = rowCells[i];
        row.classList.remove("popdoc-row-skipped");
        cell.classList.add("popdoc-result-err");
        cell.textContent = formatError(error);
      }
      output.classList.add("popdoc-expanded");

      const errorTop = instantiate(TPL_ERROR_TOP);
      errorTop.querySelector(".popdoc-cell-snippet").textContent = expr.snippet;
      errorTop.querySelector(".popdoc-cell-result").textContent = formatError(error);
      topResult.replaceWith(errorTop);
      topResult = errorTop;
    },
    appendStdio(entries) {
      const stdout = entries.flatMap((e) => e.logs.stdout).join("");
      const stderr = entries.flatMap((e) => e.logs.stderr).join("");
      if (stdout.length > 0) top.appendChild(stdioRow("STDOUT", stdout));
      if (stderr.length > 0) top.appendChild(stdioRow("STDERR", stderr, "popdoc-stderr"));
    },
  };
}

function renderStatus(status, { error, elapsedMs }) {
  const node = instantiate(TPL_STATUS);
  const hasError = error !== null;
  node.querySelector(".popdoc-dot").classList.add(hasError ? "popdoc-dot-err" : "popdoc-dot-ok");
  node.lastElementChild.textContent = `${hasError ? "error" : "ok"} · ${elapsedMs} ms`;
  status.hidden = false;
  status.replaceChildren(...node.children);
}

function stdioRow(labelText, body, cls = null) {
  const row = instantiate(TPL_STDIO);
  if (cls !== null) row.classList.add(cls);
  row.querySelector(".popdoc-label").textContent = labelText;
  row.lastElementChild.textContent = body;
  return row;
}

function decorateBlocks() {
  let blockIndex = 0;
  const blocks = [];

  for (const codeEl of document.querySelectorAll(EVAL_BLOCK_SEL)) {
    const preEl = codeEl.parentElement;
    if (preEl === null || preEl.dataset.popdocProcessed === "true") continue;

    preEl.dataset.popdocProcessed = "true";
    const blockId = preEl.id.length > 0 ? preEl.id : `popdoc-eval-${++blockIndex}`;

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

window.addEventListener("exdoc:loaded", async () => {
  const blocks = decorateBlocks();
  const popcorn = await initPopcorn();
  window.popcorn = popcorn;
  addClickHandlers(popcorn, blocks);
});
