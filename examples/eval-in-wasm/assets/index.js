import { Popcorn } from "@swmansion/popcorn";

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
  const result = await Popcorn.init({
    beam: { manifestUrl: "/assets/otp/manifest.json" },
    onStdout: (text) => displayLog(text, { isError: false }),
    onStderr: (text) => displayLog(text, { isError: true }),
  });
  if (!result.ok) throw result.error;
  const popcorn = result.data;

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

  document.documentElement.dataset.popcornReady = "true";
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

async function sendEvalRequest(popcorn, code) {
  if (code === "") {
    return;
  }

  Elements.stateDisplay.textContent = "Evaluating...";
  Elements.logsDisplay.innerHTML = "";

  try {
    const action = getEvalAction(code);
    const start = performance.now();
    const result = await popcorn.genserver.call("main", [action, code], {
      timeoutMs: 10_000,
    });
    const durationMs = performance.now() - start;
    if (!result.ok) throw result.error;
    if (result.data.error) throw new Error(result.data.error);
    Elements.stateDisplay.textContent = "Done.";
    Elements.timeDisplay.textContent = `${durationMs.toFixed(3)} ms`;
    Elements.resultDisplay.textContent = result.data.value;
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
