import { Popcorn } from "./wasm/popcorn.js";

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;
const LANGUAGE_ERLANG = "erlang";
const LANGUAGE_ELIXIR = "elixir";

var term = new Terminal();
term.open(document.getElementById(LANGUAGE + '_terminal'));
window.terminal = term;

async function setup() {
  const popcorn = await Popcorn.init({
    bundlePath: "wasm/app.avm",
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
    console.log(key);
    let code_bunch = key.key;
    console.log(key.domEvent.keyCode);
    let keyCode = key.domEvent.keyCode;
    if (keyCode === 38 || keyCode === 40) {
        code_bunch = '';
    }
    try {
      const { data, durationMs } = await popcorn.call({"command": "code_data", "language": LANGUAGE, "code": code_bunch}, {
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
    console.log(log)
  }
  window.terminal.write(log);
}

await setup();
