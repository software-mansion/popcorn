const LOG_TIMEOUT_MS = 1000;

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;
const EVAL_ELIXIR = "eval:elixir";
const EVAL_ERLANG = "eval:erlang";
const EVAL_ERLANG_MODULE = "eval_module:erlang";

var Module = {
  arguments: ["app.avm"],
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
  get exampleModuleButton() {
    return document.getElementById("example-module");
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
};

let EXAMPLES;
if (LANGUAGE === "elixir") {
  EXAMPLES = {
    CASE: `
case Enum.max([1, 2, 3]) do
  3 -> {:ok, 3}
  2 -> {:error, 2}
end
`.trim(),

    MODULE: `
defmodule Adder do
  def add(a,b) do
    a + b
  end
end

{:sum, Adder.add(10, 20)}
`.trim(),
  };
} else {
  EXAMPLES = {
    CASE: `
case lists:max([1,3,2]) of
  3 -> {ok, 3};
  2 -> {error, 2}
end.
`.trim(),

    MODULE: `
-module(basic).
-export([add/2]).

add(A, B) -> A + B.
`.trim(),
  };
}

function setup() {
  Elements.exampleCaseButton.onclick = () => {
    setExample(EXAMPLES.CASE);
  };
  Elements.exampleModuleButton.onclick = () => {
    setExample(EXAMPLES.MODULE);
  };

  if (LANGUAGE === "erlang") {
    Elements.moduleButton.onclick = () => {
      const code = Elements.moduleInput.value.trim();
      evalCode(code);
    };
  }

  Elements.clearButton.onclick = () => {
    Elements.logsDisplay.innerHTML = "";
  };
  Elements.evalButton.onclick = () => {
    const code = Elements.codeInput.value.trim();
    evalCode(code);
  };
  Elements.codeInput.addEventListener("keydown", (event) => {
    const cmdEnter = event.key === "Enter" && (event.metaKey || event.ctrlKey);
    if (cmdEnter) {
      const code = Elements.codeInput.value.trim();
      evalCode(code);
    }
  });
}

function isErlangModule(code) {
  return code.startsWith("-module(");
}

function setExample(code) {
  if (LANGUAGE === "erlang" && isErlangModule(code)) {
    Elements.moduleInput.value = code;
  } else {
    Elements.codeInput.value = code;
  }
}

async function evalCode(code) {
  if (code === "") {
    return;
  }

  let command;
  if (LANGUAGE === "elixir") {
    command = `${EVAL_ELIXIR}:${code}`;
  } else if (isErlangModule(code)) {
    command = `${EVAL_ERLANG_MODULE}:${code}`;
  } else {
    command = `${EVAL_ERLANG}:${code}`;
  }

  Elements.stateDisplay.textContent = "Evaluating...";

  try {
    const { result, dtMs } = await profile(async () =>
      Module.call("main", command),
    );
    Elements.stateDisplay.textContent = "Done.";
    console.log(`Took ${dtMs} ms, result: ${result}`);

    Elements.timeDisplay.textContent = `${dtMs.toFixed(3)} ms`;
    Elements.resultDisplay.textContent = result;
  } catch {
    Elements.stateDisplay.textContent = "Evaluation error!";
  }
}

async function profile(fn) {
  const t = performance.now();
  const result = await fn();
  const dtMs = performance.now() - t;

  return { result, dtMs };
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

setup();
