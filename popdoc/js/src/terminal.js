import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { instantiate, TPL_TERMINAL } from "./templates.js";
import {
  startLogCapture,
  ensureTrailingNewline,
  errorMessage,
  EVAL_TIMEOUT_MS,
} from "./eval.js";
import { getPopcorn, reinitPopcorn } from "./popdoc.js";

let terminalEl = null;
let term = null;
let fitAddon = null;
let collapseBtn = null;
let resizeObserver = null;

// JS owns the whole input line: echo, prompt rendering, and the continuation
// buffer. Elixir only ever sees complete submissions via ["iex_eval", code]
// and replies with a structured {status, result|error, prompt} — there is no
// tty stream anymore.
let promptNumber = 1;
let currentLine = "";
let pendingLines = [];
let evalChain = Promise.resolve();
let evalInFlight = false;
// True while the session has a live prompt on screen; typing is ignored
// between a reset and the restarted session's first prompt.
let sessionReady = false;
// Bumped on reset; evals submitted before a reset must not run against the
// new session or write into the fresh terminal.
let terminalGeneration = 0;

// Magenta maps to the popdoc purple accent in both terminal themes, so the
// prompt stands apart from typed input (default foreground) and results
// (syntax-colored by Elixir's inspect).
const PROMPT_STYLE = "\x1b[1;35m";
const CONT_PROMPT_STYLE = "\x1b[2;35m";

function promptText() {
  return `${PROMPT_STYLE}iex(${promptNumber})>\x1b[0m `;
}

function contPromptText() {
  return `${CONT_PROMPT_STYLE}...(${promptNumber})>\x1b[0m `;
}

export function initTerminalPrompt(n) {
  promptNumber = n;
  currentLine = "";
  pendingLines = [];
  sessionReady = true;
  term?.write(promptText());
}

export function resetTerminalState() {
  terminalGeneration += 1;
  promptNumber = 1;
  currentLine = "";
  pendingLines = [];
  evalInFlight = false;
  sessionReady = false;
}

export function getTerminalGeneration() {
  return terminalGeneration;
}

function toCrlf(text) {
  return text.replace(/\r?\n/g, "\r\n");
}

function writeLogs(logs) {
  for (const message of logs.stdout) {
    term.write(toCrlf(ensureTrailingNewline(message)));
  }
  for (const message of logs.stderr) {
    term.write(`\x1b[31m${toCrlf(ensureTrailingNewline(message))}\x1b[0m`);
  }
}

function writeError(error) {
  const label =
    error?.type != null ? error.type : (error?.kind ?? "error");
  const message = error?.message ?? "unknown error";
  term.write(`\x1b[31m** (${label}) ${toCrlf(message)}\x1b[0m\r\n`);
  if (error?.stacktrace && error.stacktrace.length > 0) {
    term.write(`\x1b[2m${toCrlf(error.stacktrace)}\x1b[0m\r\n`);
  }
}

function reportEvalFailure(reason) {
  pendingLines = [];
  term.write(`\x1b[31m${toCrlf(reason)}\x1b[0m\r\n`);
  term.write(promptText());
}

// Red line set apart by blank lines, for popdoc's own messages (as opposed
// to evaluation output).
export function writeSystemError(message) {
  term?.write(`\r\n\x1b[31m${message}\x1b[0m\r\n`);
}

function interruptLine() {
  term.write("\x1b[2m^C\x1b[0m\r\n");
  currentLine = "";
  pendingLines = [];
  term.write(promptText());
}

// Serialize evals: typed input and markdown iex> clicks share one session,
// so they must never interleave.
function enqueueEval(fn) {
  // evalChain is masked below and never rejects, so no rejection handler.
  const run = evalChain.then(fn);
  evalChain = run.then(
    () => {},
    () => {},
  );
  return run;
}

const STALE_OUTCOME = { ok: false, reason: "runtime was reset", stale: true };

// Evaluates `code` assuming the cursor sits at the start of a fresh line
// (the caller already echoed the input). Owns ALL failure rendering — callers
// never print their own error line — and returns
// {ok, reason?, incomplete?, stale?}. `gen` is the generation captured when
// the eval was submitted, so work queued before a reset dies silently.
async function evalOnce(code, gen) {
  if (gen !== terminalGeneration) return STALE_OUTCOME;
  evalInFlight = true;
  try {
    const stopLogCapture = startLogCapture();
    let result;
    try {
      result = await getPopcorn().call(["iex_eval", code], {
        timeoutMs: EVAL_TIMEOUT_MS,
      });
    } catch (error) {
      stopLogCapture();
      if (gen !== terminalGeneration) return STALE_OUTCOME;
      const reason = errorMessage(error);
      if (term) reportEvalFailure(reason);
      return { ok: false, reason };
    }
    const logs = stopLogCapture();

    if (gen !== terminalGeneration) return STALE_OUTCOME;

    if (!result.ok) {
      const reason = errorMessage(result.error);
      reportEvalFailure(reason);
      return { ok: false, reason };
    }

    const data = result.data ?? {};
    writeLogs(logs);

    if (data.status === "incomplete") {
      term.write(contPromptText());
      return { ok: false, reason: "incomplete expression", incomplete: true };
    }

    promptNumber = data.prompt ?? promptNumber + 1;
    pendingLines = [];

    if (data.status === "ok") {
      term.write(`${data.result}\r\n`);
      term.write(promptText());
      return { ok: true };
    }

    writeError(data.error);
    term.write(promptText());
    return { ok: false, reason: data.error?.message ?? "evaluation failed" };
  } finally {
    // A stale eval must not unlock input: after a reset the flag belongs to
    // the new generation, which may have its own eval in flight.
    if (gen === terminalGeneration) evalInFlight = false;
  }
}

async function submitLine() {
  if (pendingLines.length === 0 && currentLine.trim().length === 0) {
    currentLine = "";
    term.write(promptText());
    return;
  }
  pendingLines.push(currentLine);
  currentLine = "";
  const code = pendingLines.join("\n");
  const gen = terminalGeneration;
  await enqueueEval(() => evalOnce(code, gen));
}

// Runs a markdown iex> command inside the terminal: discards any half-typed
// input, echoes the command as if typed, evaluates it, and reports a
// structured outcome for the per-prompt status UI.
export function runSnippetInTerminal(code) {
  const gen = terminalGeneration;
  return enqueueEval(async () => {
    if (!term) return { ok: false, reason: "terminal is not open" };
    if (gen !== terminalGeneration) return STALE_OUTCOME;

    if (currentLine.length > 0 || pendingLines.length > 0) {
      interruptLine();
    }

    const lines = code.split("\n");
    term.write(`${lines[0]}\r\n`);
    for (const line of lines.slice(1)) {
      term.write(`${contPromptText()}${line}\r\n`);
    }

    const outcome = await evalOnce(code, gen);
    if (outcome.incomplete) {
      // A markdown command is a complete unit; an incomplete one is a doc
      // bug. Report it and restore a clean prompt.
      pendingLines = [];
      writeSystemError(`popdoc: incomplete command in the docs: \`${lines[0]}\``);
      term.write(promptText());
    }
    return outcome;
  });
}

export function getTerm() {
  return term;
}

function isDarkMode() {
  return document.body.classList.contains("dark");
}

// The palette lives in popdoc.css as --popdoc-term-* custom properties
// (light values plus body.dark overrides); xterm cannot read CSS, so this
// bridges the computed values into its theme object. Keys missing from the
// CSS fall back to xterm defaults.
const THEME_KEYS = [
  "background",
  "foreground",
  "cursor",
  "cursorAccent",
  "selectionBackground",
  "black",
  "red",
  "green",
  "yellow",
  "blue",
  "magenta",
  "cyan",
  "white",
  "brightBlack",
  "brightRed",
  "brightGreen",
  "brightYellow",
  "brightBlue",
  "brightMagenta",
  "brightCyan",
  "brightWhite",
];

function themeVar(key) {
  return `--popdoc-term-${key.replace(/[A-Z]/g, (c) => `-${c.toLowerCase()}`)}`;
}

function terminalTheme() {
  const styles = getComputedStyle(document.body);
  const theme = {};
  for (const key of THEME_KEYS) {
    const value = styles.getPropertyValue(themeVar(key)).trim();
    if (value.length > 0) theme[key] = value;
  }
  return theme;
}

function terminalOptions() {
  const styles = getComputedStyle(document.body);
  const mono =
    styles.getPropertyValue("--monoFontFamily").trim() || "monospace";

  return {
    cursorBlink: true,
    scrollback: 10000,
    smoothScrollDuration: 100,
    fontFamily: mono,
    theme: terminalTheme(),
  };
}

function observeResize(mount) {
  if (resizeObserver) {
    resizeObserver.disconnect();
  }
  resizeObserver = new ResizeObserver(() => {
    fitAddon?.fit();
  });
  resizeObserver.observe(mount);
}

function syncTerminalTheme() {
  if (term?.options) {
    term.options.theme = terminalTheme();
  }
}

let themeObserver = null;
let lastDarkMode = null;

// ExDoc's theme toggle flips the "dark" class on <body>; xterm's theme is a
// plain options object, so it must be re-read when that happens. Other body
// class changes (e.g. opening the terminal) fire the observer too, so only
// re-theme when the dark flag actually flipped.
function observeThemeChanges() {
  if (themeObserver) return;
  lastDarkMode = isDarkMode();
  themeObserver = new MutationObserver(() => {
    const dark = isDarkMode();
    if (dark === lastDarkMode) return;
    lastDarkMode = dark;
    syncTerminalTheme();
  });
  themeObserver.observe(document.body, {
    attributes: true,
    attributeFilter: ["class"],
  });
}

function setCollapsedUi(collapsed) {
  if (!collapseBtn) return;
  collapseBtn.textContent = collapsed ? "□" : "─";
  collapseBtn.title = collapsed ? "Expand" : "Collapse";
}

// Strip whole escape sequences first (Delete/Home/End/F-keys arrive as CSI
// or SS3 sequences that would otherwise leak "[3~"-style residue once the
// bare ESC byte is removed), then remaining control bytes. Tabs survive:
// stripping them from pastes silently corrupts code and string literals.
function sanitizeSegment(seg) {
  return seg
    .replace(/\x1b(?:\[[0-?]*[ -\/]*[@-~]|O[@-~])/g, "")
    .replace(/[\x00-\x08\x0b-\x1f\x7f]/g, "");
}

export function ensureTerminal() {
  if (terminalEl && term) return;

  terminalEl = instantiate(TPL_TERMINAL);
  const mount = terminalEl.querySelector(".popdoc-terminal-mount");
  collapseBtn = terminalEl.querySelector('[data-action="collapse"]');

  terminalEl
    .querySelector('[data-action="clear"]')
    .addEventListener("click", () => {
      // Like the shell `clear`: wipe the scrollback but keep the cursor's
      // line, so the current prompt and any typed input survive.
      term?.clear();
    });

  terminalEl
    .querySelector('[data-action="reset"]')
    .addEventListener("click", async () => {
      // reset() also drops the current line with the now-stale prompt;
      // the restarted session prints a fresh one.
      if (term) term.reset();
      try {
        await reinitPopcorn();
      } catch (error) {
        console.error("popdoc: failed to restart the runtime:", error);
      }
    });

  collapseBtn.addEventListener("click", () => {
    const collapsed = terminalEl.classList.toggle("popdoc-terminal--collapsed");
    setCollapsedUi(collapsed);
    if (!collapsed && fitAddon) fitAddon.fit();
  });

  terminalEl
    .querySelector('[data-action="close"]')
    .addEventListener("click", () => {
      terminalEl.classList.remove(
        "popdoc-terminal--open",
        "popdoc-terminal--collapsed",
      );
      document.body.classList.remove("popdoc-terminal-open");
      setCollapsedUi(false);
    });

  document.body.appendChild(terminalEl);

  term = new Terminal(terminalOptions());
  fitAddon = new FitAddon();
  term.loadAddon(fitAddon);
  term.open(mount);
  fitAddon.fit();
  observeResize(mount);
  observeThemeChanges();

  term.onData(async (data) => {
    // Tab completion is not supported; swallow the key.
    if (data === "\t") return;
    // Input is locked while an eval runs (the VM is single-threaded, so
    // there is nothing useful to type into) and while a reset is rebooting
    // the session.
    if (evalInFlight || !sessionReady) return;

    if (data === "\x7f") {
      if (currentLine.length > 0) {
        // Drop the last code point — surrogate pairs are two UTF-16 units —
        // and erase its cells (astral glyphs like emoji render two columns
        // wide; BMP wide glyphs are still mis-erased by one cell).
        const chars = Array.from(currentLine);
        const removed = chars.pop();
        currentLine = chars.join("");
        term.write(removed.length === 2 ? "\b\b  \b\b" : "\b \b");
      }
      return;
    }

    if (data === "\x03") {
      interruptLine();
      return;
    }

    // A chunk may be a single keystroke or a whole paste; newlines inside it
    // submit line-by-line, so pasted snippets flow through the same
    // continuation pipeline as typed input.
    const parts = data.split(/\r\n|\r|\n/);
    for (let i = 0; i < parts.length; i++) {
      const seg = sanitizeSegment(parts[i]);
      if (seg.length > 0) {
        currentLine += seg;
        term.write(seg);
      }
      if (i < parts.length - 1) {
        term.write("\r\n");
        await submitLine();
      }
    }
  });
}

export function openTerminal() {
  if (!terminalEl) return;
  terminalEl.classList.add("popdoc-terminal--open");
  terminalEl.classList.remove("popdoc-terminal--collapsed");
  document.body.classList.add("popdoc-terminal-open");
  setCollapsedUi(false);
  if (fitAddon) fitAddon.fit();
}
