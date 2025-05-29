import { Popcorn } from "./wasm/popcorn.js";

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;
const ARROW_UP_KEY_CODE = 38;
const ARROW_DOWN_KEY_CODE = 40;

async function setup() {
  var term = new Terminal();
  term.open(document.getElementById(LANGUAGE + '-terminal'));
  window.terminal = term;

  const popcorn = await Popcorn.init({
    debug: true,
    onStdout: (text) => displayLog(text, { isError: false }),
    onStderr: (text) => displayLog(text, { isError: true }),
  });
  
  try {
    const { data, durationMs } = await popcorn.call({"command": "start", "language": LANGUAGE}, {
      timeoutMs: 10_000,
    });
  } catch (error) {
      displayLog(error, { isError: true }) 
  }
  
  window.terminal.onKey(async key => {
    const text = key.key;
    const keyCode = key.domEvent.keyCode;
    if (keyCode === ARROW_UP_KEY_CODE || keyCode === ARROW_DOWN_KEY_CODE) {
        text = '';
    }
    try {
      const { data, durationMs } = await popcorn.call({"command": "code_data", "language": LANGUAGE, "text": text}, {
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
