import { Popcorn } from "./wasm/popcorn.js";

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;
const LANGUAGE_ERLANG = "erlang";
const LANGUAGE_ELIXIR = "elixir";
const EVAL_ELIXIR = "eval_elixir";
const EVAL_ERLANG = "eval_erlang";
const EVAL_ERLANG_MODULE = "eval_erlang_module";

const Elements = {
  get evalButton() {
    return document.getElementById("eval");
  },
  get moduleButton() {
    return document.getElementById("eval-module");
  },
  get clearButton() {
    return document.getElementById("clear");
  },
  get codeInput() {
    return document.getElementById("code");
  },
  get exampleButtons() {
    return document.querySelectorAll('button[data-type="example"]');
  },
  get moduleInput() {
    return document.getElementById("module");
  },
  get stateDisplay() {
    return document.getElementById("state");
  },
  get timeDisplay() {
    return document.getElementById("time");
  },
  get resultDisplay() {
    return document.getElementById("result");
  },
  get logsDisplay() {
    return document.getElementById("logs");
  },
  get evalFrame() {
    return document.getElementById("evalFrame");
  },
};

async function setup() {
  const popcorn = await Popcorn.init({
    debug: true,
    onStdout: (text) => displayLog(text, { isError: false }),
    onStderr: (text) => displayLog(text, { isError: true }),
  });

  Elements.exampleButtons.forEach((button) => {
    button.onclick = () => {
      const isModule = button.getAttribute("data-input") === "module";
      const target = isModule ? Elements.moduleInput : Elements.codeInput;
      target.textContent = button.value.trim();
    };
  });

  Elements.clearButton.onclick = () => {
    Elements.logsDisplay.innerHTML = "";
  };

  const evalButtons = [[Elements.evalButton, Elements.codeInput]];
  if (LANGUAGE === LANGUAGE_ERLANG) {
    evalButtons.push([Elements.moduleButton, Elements.moduleInput]);
  }

  for (const [button, input] of evalButtons) {
    function evalCode() {
      const code = input.value.trim();
      sendEvalRequest(popcorn, code);
    }

    button.onclick = evalCode;
    input.addEventListener("keydown", onCmdEnter(evalCode));
  }
}

function onCmdEnter(fn) {
  return (event) => {
    const cmdEnter = event.key === "Enter" && (event.metaKey || event.ctrlKey);
    if (cmdEnter) {
      fn();
    }
  };
}

function isErlangModule(code) {
  return code.startsWith("-module(");
}

async function sendEvalRequest(/**@type {Popcorn}*/ popcorn, code) {
  if (code === "") {
    return;
  }

  Elements.stateDisplay.textContent = "Evaluating...";
  Elements.logsDisplay.innerHTML = "";

  try {
    const action = getEvalAction(code);
    const { data, durationMs } = await popcorn.call([action, code], {
      timeoutMs: 10_000,
    });
    Elements.stateDisplay.textContent = "Done.";
    Elements.timeDisplay.textContent = `${durationMs.toFixed(3)} ms`;
    Elements.resultDisplay.textContent = data;
  } catch (error) {
    Elements.stateDisplay.textContent = "Evaluation error!";
    Elements.timeDisplay.textContent = "";
    Elements.resultDisplay.textContent = "";
  }
}

function getEvalAction(code) {
  if (LANGUAGE === LANGUAGE_ELIXIR) return EVAL_ELIXIR;
  if (isErlangModule(code)) return EVAL_ERLANG_MODULE;
  return EVAL_ERLANG;
}

function displayLog(log, { isError }) {
  const lineElement = document.createElement("span");
  lineElement.textContent = log;

  if (isError) {
    lineElement.style.color = "var(--swm-red-100)";
  }

  Elements.logsDisplay.appendChild(lineElement);
  Elements.logsDisplay.scrollTo({
    top: Elements.logsDisplay.scrollHeight,
    behavior: "instant",
  });
}

await setup();
