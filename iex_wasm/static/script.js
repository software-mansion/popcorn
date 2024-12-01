const LOG_TIMEOUT_MS = 1000;
const TYPE = { EVAL: "eval", MODULE: "module" };

const CODE_EXAMPLES = {
  CASE: [
    "case lists:max([1,3,2]) of",
    "  3 -> ok;",
    "  2 -> error",
    "end.",
  ].join("\n"),
};

const MODULE_EXAMPLES = {
  BASIC: [
    "-module(basic).",
    "-export([add/2]).",
    "",
    "add(A, B) -> A + B.",
  ].join("\n"),
};

var Module = {
  arguments: ["iex_wasm.avm"],
  print(text) {
    console.log(text);
    log(text, false);
  },
  printErr(text) {
    console.error(text);
    log(text, true);
  },
};

const Elements = {
  get evalButton() {
    return document.getElementById("eval");
  },
  get moduleButton() {
    return document.getElementById("eval-module");
  },
  get exampleCaseButton() {
    return document.getElementById("example-case");
  },
  get exampleBasicModuleButton() {
    return document.getElementById("example-basic-module");
  },
  get clearButton() {
    return document.getElementById("clear");
  },
  get codeInput() {
    return document.getElementById("code");
  },
  get moduleInput() {
    return document.getElementById("module");
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
};

function setup() {
  Elements.exampleCaseButton.onclick = () => {
    Elements.codeInput.value = CODE_EXAMPLES.CASE;
  };
  Elements.exampleBasicModuleButton.onclick = () => {
    Elements.moduleInput.value = MODULE_EXAMPLES.BASIC;
  };
  Elements.clearButton.onclick = () => {
    Elements.logsDisplay.innerHTML = "";
  };
  Elements.evalButton.onclick = () => evalErlang();
  Elements.moduleButton.onclick = () => compileErlangModule();
  Elements.codeInput.addEventListener("keydown", (event) => {
    const cmdEnter = event.key === "Enter" && (event.metaKey || event.ctrlKey);
    if (cmdEnter) {
      evalCode();
    }
  });
}

function log(text, isError) {
  if (this.lines === undefined) {
    this.lines = [];
    this.errors = new Set();
    this.timer = undefined;
  }

  this.lines.push(text);
  if (isError) {
    this.errors.add(this.lines.length - 1);
  }

  clearTimeout(this.timer);
  this.timer = setTimeout(() => {
    displayLog(this.lines, this.errors);

    this.lines = [];
    this.errors.clear();
    this.timer = undefined;
  }, LOG_TIMEOUT_MS);
}

function displayLog(lines, errors) {
  const fragment = document.createDocumentFragment();
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const isError = errors.has(i);
    const lineElement = document.createElement("span");
    lineElement.textContent = line;

    if (isError) {
      lineElement.style.color = "var(--swm-red-100)";
    }
    fragment.appendChild(lineElement);
  }

  Elements.logsDisplay.appendChild(fragment);
  Elements.logsDisplay.scrollTo({
    top: Elements.logsDisplay.scrollHeight,
    behavior: "instant",
  });
}

async function evalErlang() {
  await executeErlang(Elements.codeInput.value, TYPE.EVAL);
}

async function compileErlangModule() {
  await executeErlang(Elements.moduleInput.value, TYPE.MODULE);
}

async function profile(fn) {
  const t = performance.now();
  const result = await fn();
  const dtMs = performance.now() - t;

  return { result, dtMs };
}

async function executeErlang(code, type) {
  if (code === "") {
    return;
  }

  console.log("Evaluating");
  const { result, dtMs } = await profile(async () =>
    Module.call("main", type + ":" + code)
  );
  console.log(`Took ${dtMs} ms, result: ${result}`);

  Elements.timeDisplay.textContent = `${dtMs.toFixed(3)} ms`;
  Elements.resultDisplay.textContent = result;
}

setup();
