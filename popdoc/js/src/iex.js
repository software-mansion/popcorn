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
import { getPopcorn } from "./popdoc.js";
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
  if (iexBusy) return;
  if (!iexCommands.includes(cmd)) return;

  // Claim the shell before the first await so a second click doesn't start
  // a parallel chain.
  iexBusy = true;
  try {
    await startIexSession();
    openTerminal();

    const xterm = getTerm();

    if (cmd.state === "success") {
      xterm?.scrollToBottom?.();
      return;
    }

    const gen = getTerminalGeneration();
    for (const current of getIexCommandsToRun(cmd)) {
      // Delay the spinner so fast commands don't flicker.
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
      // The terminal already rendered the failure.
      if (!outcome.ok) break;
    }
  } finally {
    iexBusy = false;
  }
}

// Plain iex blocks (no `iex-popcorn` fence) get the runnable block's gutter so
// `iex>` sits at the same column in both, even though only one is clickable.
function alignPlainIexBlocks() {
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
      // Prompt spans sit on separate lines, so CSS :hover cannot cover the
      // whole command.
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
  // Concurrent callers share one start_iex call; a second one would print
  // a second prompt.
  if (!startPromise) {
    startPromise = (async () => {
      const gen = getTerminalGeneration();
      ensureTerminal();
      const result = await getPopcorn().call(["start_iex"], {
        timeoutMs: EVAL_TIMEOUT_MS,
      });
      // A reset happened while start_iex was in flight; leave the new
      // session alone.
      if (gen !== getTerminalGeneration()) return;
      if (!result.ok) {
        throw new Error(`Failed to start IEx: ${errorMessage(result.error)}`);
      }
      iexReady = true;
      initTerminalPrompt();
    })();
  }
  try {
    await startPromise;
  } finally {
    // Clear so a failed start can be retried on the next call.
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
