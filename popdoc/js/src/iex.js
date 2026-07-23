import {
  ensureTerminal,
  openTerminal,
  getTerm,
  initTerminalPrompt,
  resetTerminalState,
  runSnippetInTerminal,
  getTerminalGeneration,
} from "./terminal.js";
import { EVAL_TIMEOUT_MS, errorMessage } from "./eval.js";
import { getPopcorn, initPopcorn } from "./popdoc.js";
import { instantiate, TPL_IEX_ICON } from "./templates.js";

const IEX_BLOCK_SEL = "pre.popcorn-iex";
const RUNNING_INDICATOR_MS = 200;

const IEX_PROMPT_RE = /^iex(?:\(\d+\))?>/;
const CONT_PROMPT_RE = /^\.\.\.(?:\(\d+\))?>/;

let iexBusy = false;

export const iexCommands = [];
let iexReady = false;
let startPromise = null;

function getBlockCommands(blockEl) {
  return iexCommands.filter((cmd) => cmd.blockEl === blockEl);
}

function cmdEls(cmd) {
  return [cmd.promptEl, ...cmd.contEls];
}

function setCmdState(cmd, state) {
  cmd.state = state ?? "not_run";
  for (const el of cmdEls(cmd)) {
    if (state) {
      el.setAttribute("data-iex-state", state);
    } else {
      el.removeAttribute("data-iex-state");
    }
  }
}

function updateBlockRunnable(blockEl) {
  const cmds = getBlockCommands(blockEl);
  const next = cmds.find((cmd) => cmd.state !== "success");
  for (const cmd of cmds) {
    for (const el of cmdEls(cmd)) {
      el.classList.toggle("popdoc-iex-prompt--runnable", cmd === next);
    }
  }
}

function updateAllRunnable() {
  const blocks = new Set(iexCommands.map((cmd) => cmd.blockEl));
  for (const blockEl of blocks) {
    updateBlockRunnable(blockEl);
  }
}

function getIexCommandsToRun(cmd) {
  const blockCmds = getBlockCommands(cmd.blockEl);
  const index = blockCmds.indexOf(cmd);
  if (index < 0) return [];

  const pending = blockCmds.findIndex((c) => c.state !== "success");
  const first = pending >= 0 && pending < index ? pending : index;
  return blockCmds.slice(first, index + 1);
}

async function runIex(cmd) {
  // One shell, one command at a time — parallel runs would interleave input.
  if (iexBusy) return;
  if (!iexCommands.includes(cmd)) return;

  // Claim the shell before the first await: a second click during session
  // startup must not run a parallel copy of the chain.
  iexBusy = true;
  try {
    // Idempotent; revives the shell if a reset or reload tore it down.
    await startIexSession();
    openTerminal();

    const xterm = getTerm();

    if (cmd.state === "success") {
      xterm?.scrollToBottom?.();
      return;
    }

    const gen = getTerminalGeneration();
    for (const current of getIexCommandsToRun(cmd)) {
      // Fast commands go straight to their result; only a slow one gets a
      // spinner, so the block does not flicker on every click.
      const showRunning = setTimeout(() => {
        // A reset may have cleared all states while we waited.
        if (gen === getTerminalGeneration()) setCmdState(current, "running");
      }, RUNNING_INDICATOR_MS);

      let outcome;
      try {
        outcome = await runSnippetInTerminal(current.code.trimEnd());
      } catch (error) {
        outcome = { ok: false, reason: errorMessage(error) };
      } finally {
        clearTimeout(showRunning);
      }

      // Commands sent before a reset must not mark state on the new shell.
      if (gen !== getTerminalGeneration() || outcome.stale) return;

      setCmdState(current, outcome.ok ? "success" : "failure");
      updateBlockRunnable(current.blockEl);
      // Failures are already rendered by the terminal eval path; just stop
      // the chain.
      if (!outcome.ok) break;
    }
  } finally {
    iexBusy = false;
  }
}

// Plain iex blocks (no `iex-popcorn` fence) get the runnable block's gutter so
// `iex>` sits at the same column in both, even though only one is clickable.
function alignPlainIexBlocks() {
  // One document-wide pass: most <pre> blocks carry no prompt span at all, so
  // matching .gp directly skips them instead of querying inside each block.
  const seen = new Set();
  for (const gpEl of document.querySelectorAll("pre:not(.popcorn-iex) .gp")) {
    const preEl = gpEl.closest("pre");
    if (seen.has(preEl)) continue;
    seen.add(preEl);
    if (IEX_PROMPT_RE.test(gpEl.textContent.trimStart())) {
      preEl.classList.add("popdoc-iex-aligned");
    }
  }
}

export function decorateIexBlocks() {
  alignPlainIexBlocks();

  for (let i = iexCommands.length - 1; i >= 0; i--) {
    if (!document.contains(iexCommands[i].promptEl)) {
      iexCommands.splice(i, 1);
    }
  }

  for (const preEl of document.querySelectorAll(IEX_BLOCK_SEL)) {
    if (preEl.dataset.popdocIexProcessed === "true") continue;
    preEl.dataset.popdocIexProcessed = "true";

    const commandsJson = preEl.dataset.popcornIexCommands;
    if (!commandsJson) continue;

    let commands;
    try {
      commands = JSON.parse(commandsJson);
    } catch (error) {
      console.warn(
        "popdoc: unreadable iex command list, block left inert",
        preEl,
        error,
      );
      continue;
    }

    // Makeup marks both "iex>" and "...>" prompts as .gp spans; group each
    // "iex>" with its continuation lines so the whole command reacts as one.
    const gpEls = [...preEl.querySelectorAll(".gp")].map((gpEl) => ({
      gpEl,
      text: gpEl.textContent.trimStart(),
    }));
    const promptCount = gpEls.filter(({ text }) =>
      IEX_PROMPT_RE.test(text),
    ).length;

    if (promptCount !== commands.length) {
      console.warn(
        "popdoc: iex command/prompt mismatch, block left inert",
        preEl,
      );
      continue;
    }

    let currentCmd = null;
    let commandIndex = 0;
    for (const { gpEl, text } of gpEls) {
      if (IEX_PROMPT_RE.test(text)) {
        gpEl.classList.add("popdoc-iex-prompt");
        gpEl.title = "Run in IEx";
        gpEl.setAttribute("aria-label", "Run in IEx");
        gpEl.setAttribute("role", "button");
        gpEl.tabIndex = 0;
        gpEl.prepend(instantiate(TPL_IEX_ICON));
        currentCmd = {
          code: commands[commandIndex],
          promptEl: gpEl,
          contEls: [],
          blockEl: preEl,
          state: "not_run",
        };
        iexCommands.push(currentCmd);
        commandIndex += 1;
      } else if (CONT_PROMPT_RE.test(text) && currentCmd) {
        gpEl.classList.add("popdoc-iex-prompt", "popdoc-iex-prompt--cont");
        gpEl.title = "Run in IEx";
        currentCmd.contEls.push(gpEl);
      } else {
        currentCmd = null;
      }
    }

    updateBlockRunnable(preEl);
  }
}

export function addIexClickHandlers() {
  for (const cmd of iexCommands) {
    const { promptEl } = cmd;
    if (promptEl.dataset.popdocIexBound === "true") continue;
    promptEl.dataset.popdocIexBound = "true";
    const run = () => {
      runIex(cmd).catch((error) => {
        console.error("popdoc: failed to run the iex command:", error);
      });
    };
    const els = cmdEls(cmd);
    const setHover = (hovered) => {
      for (const el of els) {
        el.classList.toggle("popdoc-iex-hover", hovered);
      }
      const chain = new Set(
        hovered ? getIexCommandsToRun(cmd).filter((c) => c !== cmd) : [],
      );
      for (const other of getBlockCommands(cmd.blockEl)) {
        const on = chain.has(other);
        for (const el of cmdEls(other)) {
          el.classList.toggle("popdoc-iex-hover-chain", on);
        }
      }
    };
    for (const el of els) {
      el.addEventListener("click", run);
      // Prompt spans of one command sit on separate lines, so CSS :hover
      // cannot link them; mirror hover state with a shared class instead.
      el.addEventListener("mouseenter", () => setHover(true));
      el.addEventListener("mouseleave", () => setHover(false));
    }
    promptEl.addEventListener("keydown", (event) => {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        run();
      }
    });
  }
}

export async function startIexSession() {
  if (iexReady) return;
  // Concurrent callers (page load vs launcher click, double Reset) must
  // share one start_iex call — a second one would print a second prompt.
  if (!startPromise) {
    startPromise = (async () => {
      // Lazy boot: the first prompt/launcher interaction initializes the
      // runtime. A failed boot throws before any terminal DOM is created.
      await initPopcorn();
      const gen = getTerminalGeneration();
      ensureTerminal();
      const result = await getPopcorn().call(["start_iex"], {
        timeoutMs: EVAL_TIMEOUT_MS,
      });
      // A reset while start_iex was in flight owns the terminal now.
      if (gen !== getTerminalGeneration()) return;
      if (!result.ok) {
        throw new Error(`Failed to start IEx: ${errorMessage(result.error)}`);
      }
      iexReady = true;
      // JS draws the prompt now; the session only reports its counter.
      initTerminalPrompt(result.data?.prompt ?? 1);
    })();
  }
  try {
    await startPromise;
  } finally {
    // Failed attempts retry on the next call; success short-circuits above.
    startPromise = null;
  }
}

export function resetIexSession() {
  iexReady = false;
  startPromise = null;
  resetTerminalState();
  for (const cmd of iexCommands) {
    setCmdState(cmd, null);
  }
  updateAllRunnable();
}
