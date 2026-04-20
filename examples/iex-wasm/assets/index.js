import { Popcorn } from "@swmansion/popcorn";
import { init, Terminal, FitAddon } from "ghostty-web";

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;
const IGNORED_SEQUENCES = new Set(['\x1b[A', '\x1b[B', '\t']); // arrow up, arrow down, tab

async function setup() {
  await init();
  const terminalElement = document.getElementById(LANGUAGE + '-terminal');
  const term = new Terminal({
    cursorBlink: true,
    scrollback: 10000,
    smoothScrollDuration: 100,
  });
  const fitAddon = new FitAddon();
  term.loadAddon(fitAddon);
  term.open(terminalElement);
  fitAddon.fit();
  fitAddon.observeResize();
  window.terminal = term;

  const focusTerminal = () => {
    term.focus();
    term.textarea?.focus();
  };

  terminalElement.addEventListener('mousedown', focusTerminal);
  terminalElement.addEventListener('touchstart', focusTerminal, { passive: true });
  focusTerminal();

  const popcorn = await Popcorn.init({
    debug: true,
    bundlePaths: ["/wasm/bundle.avm"],
    onStdout: (text) => displayLog(text, { isError: false }),
    onStderr: (text) => displayLog(text, { isError: true }),
  });

  try {
    const { data, durationMs } = await popcorn.call({ "command": "start", "language": LANGUAGE }, {
      timeoutMs: 10_000,
    });
    focusTerminal();
  } catch (error) {
    displayLog(error, { isError: true })
  }

  window.terminal.onData(async data => {
    if (IGNORED_SEQUENCES.has(data)) return;
    try {
      const { data: _result, durationMs } = await popcorn.call({ "command": "code_data", "language": LANGUAGE, "text": data }, {
        timeoutMs: 10_000,
      });
    } catch (error) {
      displayLog(error, { isError: true })
    }
  });
}

function displayLog(log, { isError }) {
  if (!log) return;
  if (isError) {
    log = '\x1b[31m' + log + '\x1b[0m\n\r';
  }
  window.terminal.write(log);
}

await setup();
