import { Popcorn } from "@swmansion/popcorn";
import { init, Terminal, FitAddon, OSC8LinkProvider, UrlRegexProvider } from "ghostty-web";

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;
const ARROW_UP_KEY_CODE = 38;
const ARROW_DOWN_KEY_CODE = 40;
const TAB_KEY_CODE = 9;
const IGNORED_KEYS = [ARROW_UP_KEY_CODE, ARROW_DOWN_KEY_CODE, TAB_KEY_CODE]

async function setup() {
  await init();
  const term = new Terminal({
    cursorBlink: true,
    scrollback: 10000,
    smoothScrollDuration: 100,
  });
  const fitAddon = new FitAddon();
  term.loadAddon(fitAddon);
  term.open(document.getElementById(LANGUAGE + '-terminal'));
  fitAddon.fit();
  fitAddon.observeResize();
  term.registerLinkProvider(new OSC8LinkProvider());
  term.registerLinkProvider(new UrlRegexProvider());
  window.terminal = term;

  const popcorn = await Popcorn.init({
    debug: true,
    bundlePath: "/wasm/bundle.avm",
    onStdout: (text) => displayLog(text, { isError: false }),
    onStderr: (text) => displayLog(text, { isError: true }),
  });

  try {
    const { data, durationMs } = await popcorn.call({ "command": "start", "language": LANGUAGE }, {
      timeoutMs: 10_000,
    });
  } catch (error) {
    displayLog(error, { isError: true })
  }

  window.terminal.onKey(async key => {
    let text = key.key;
    const keyCode = key.domEvent.keyCode;
    if (IGNORED_KEYS.includes(keyCode)) {
      text = '';
    }
    try {
      const { data, durationMs } = await popcorn.call({ "command": "code_data", "language": LANGUAGE, "text": text }, {
        timeoutMs: 10_000,
      });
    } catch (error) {
      displayLog(error, { isError: true })
    }
  });
}

function displayLog(log, { isError }) {
  const lineElement = document.createElement("span");
  lineElement.textContent = log;
  if (isError) {
    log = '\x1b[31m' + log + '\x1b[0m\n\r';
  }
  window.terminal.write(log);
}

await setup();
