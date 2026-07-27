import {
  instantiate,
  TPL_SKELETON,
  TPL_TOGGLE,
  TPL_TOP,
  TPL_STDIO,
  TPL_ROW,
  TPL_ERROR_TOP,
  TPL_STACKTRACE_TOGGLE,
  TPL_STACKTRACE,
  TPL_STATUS,
} from "./templates.js";
import { getPopcorn } from "./popdoc.js";

const PENDING_DELAY_MS = 200;
export const EVAL_TIMEOUT_MS = 30_000;

export function errorMessage(error) {
  return String(error?.message ?? error);
}

export function ensureTrailingNewline(text) {
  return text.endsWith("\n") ? text : text + "\n";
}

export function hasMultipleExpressions(code) {
  return (
    code
      .trim()
      .split(/\n+/)
      .filter((l) => l.trim().length > 0).length > 1
  );
}

export function displayResult({ result, bindings }) {
  if (!bindings || bindings.length === 0) return result;
  return bindings.map((b) => `${b.name} = ${b.value}`).join(", ");
}

export function formatError(error) {
  if (error === null) return "";
  return error.type !== null
    ? `${error.type}: ${error.message}`
    : error.message;
}

export async function runCode(block) {
  const popcorn = getPopcorn();
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

  const parsed = await popcorn.call(["parse_elixir", code, blockId], {
    timeoutMs: remaining(),
  });

  clearTimeout(pendingTimer);
  output.classList.remove("popdoc-output-pending");

  if (!parsed.ok) {
    const parseError = {
      kind: "error",
      type: null,
      message: parsed.error.message,
      stacktrace: "",
    };
    output.hidden = false;
    output.replaceChildren();
    const top = instantiate(TPL_TOP);
    const resultRow = top.querySelector(".popdoc-result");
    resultRow.classList.add("popdoc-result-err");
    resultRow.textContent = formatError(parseError);
    output.appendChild(top);
    renderStatus(status, {
      error: parseError,
      elapsedMs: Math.round(performance.now() - startedAt),
    });
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
      error = {
        kind: "error",
        type: null,
        message: `Evaluation timed out after ${EVAL_TIMEOUT_MS}ms`,
        stacktrace: "",
      };
      scaffold.markError(i, expressions[i], error);
      break;
    }

    const stopLogCapture = startLogCapture();
    const step = await popcorn.call(["eval_one", blockId, i], {
      timeoutMs: budget,
    });
    const logs = stopLogCapture();

    if (!step.ok) {
      error = {
        kind: "error",
        type: null,
        message: step.error.message,
        stacktrace: "",
      };
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

    scaffold.markSuccess(i, displayResult(step.data));
  }

  if (error === null && entries.length > 0) {
    scaffold.finalize(displayResult(entries[entries.length - 1]));
  }
  scaffold.appendStdio(entries);
  renderStatus(status, {
    error,
    elapsedMs: Math.round(performance.now() - startedAt),
  });
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
      const errorTop = instantiate(TPL_ERROR_TOP);
      errorTop.querySelector(".popdoc-cell-snippet").textContent = expr.snippet;
      errorTop.querySelector(".popdoc-cell-result").textContent =
        formatError(error);
      topResult.replaceWith(errorTop);
      topResult = errorTop;

      if (error.stacktrace && error.stacktrace.length > 0) {
        const toggle = instantiate(TPL_STACKTRACE_TOGGLE);
        const trace = instantiate(TPL_STACKTRACE);
        trace.textContent = error.stacktrace;
        const chev = toggle.querySelector(".popdoc-chev");
        const label = toggle.lastElementChild;
        toggle.addEventListener("click", () => {
          const shown = trace.hidden;
          trace.hidden = !shown;
          chev.textContent = shown ? "▾" : "▸";
          label.textContent = shown ? "hide stacktrace" : "show stacktrace";
        });
        top.appendChild(toggle);
        top.appendChild(trace);
      }
    },
    appendStdio(entries) {
      const stdout = joinLogs(entries.flatMap((e) => e.logs.stdout));
      const stderr = joinLogs(entries.flatMap((e) => e.logs.stderr));
      if (stdout.length > 0) top.appendChild(stdioRow("STDOUT", stdout));
      if (stderr.length > 0)
        top.appendChild(stdioRow("STDERR", stderr, "popdoc-stderr"));
    },
  };
}

function renderStatus(status, { error, elapsedMs }) {
  const node = instantiate(TPL_STATUS);
  const hasError = error !== null;
  node
    .querySelector(".popdoc-dot")
    .classList.add(hasError ? "popdoc-dot-err" : "popdoc-dot-ok");
  node.lastElementChild.textContent = `${hasError ? "error" : "ok"} · ${elapsedMs} ms`;
  status.hidden = false;
  status.replaceChildren(...node.children);
}

function stdioRow(labelText, body, cls = null) {
  const row = instantiate(TPL_STDIO);
  if (cls !== null) row.classList.add(cls);
  row.querySelector(".popdoc-label").textContent = labelText;
  const bodyEl = row.lastElementChild;
  bodyEl.classList.add("popdoc-stdio-body");
  bodyEl.textContent = body;
  return row;
}

// Popcorn's log listener may fire per-line without the trailing newline,
// or per-write with the newline kept. Normalize so each entry ends with "\n",
// then drop any trailing newline once joined.
function joinLogs(messages) {
  if (messages.length === 0) return "";
  const joined = messages.map(ensureTrailingNewline).join("");
  return joined.endsWith("\n") ? joined.slice(0, -1) : joined;
}

export function startLogCapture() {
  const popcorn = getPopcorn();
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
