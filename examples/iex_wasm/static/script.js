import { Popcorn } from "./wasm/popcorn.js";

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;
const LANGUAGE_ERLANG = "erlang";
const LANGUAGE_ELIXIR = "elixir";

var term = new Terminal();
term.open(document.getElementById('terminal'));
window.elixir_terminal = term;

async function setup() {
  const popcorn = await Popcorn.init({
    bundlePath: "wasm/app.avm",
    debug: true,
    onStdout: (text) => displayLog(text, { isError: false }),
    onStderr: (text) => displayLog(text, { isError: true }),
  });
  
  window.elixir_terminal.onKey(async key => {
    console.log(key);
    let ansiRegex = /[^\x20-\x7F\n\b\r]/g;
    let code_bunch = key.key.replace(ansiRegex, '');

    try {
      const { data, durationMs } = await popcorn.call([LANGUAGE, code_bunch], {
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
  window.elixir_terminal.write(log);
}

await setup();
