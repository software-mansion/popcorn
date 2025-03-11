const LOG_TIMEOUT_MS = 1000;

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;
const EVAL_ELIXIR = "eval:elixir";
const EVAL_ERLANG = "eval:erlang";
const EVAL_ERLANG_MODULE = "eval_module:erlang";

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
  get evalFrame() {
    return document.getElementById("evalFrame");
  }
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
  window.addEventListener("message", (e) => {
    if (e.data.type == "log") {
      displayLog(e.data.value, false);
    } else if (e.data.type == "log_error") {
      displayLog(e.data.value, true);
    }
  });

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
  Elements.logsDisplay.innerHTML = "";

  do {
    Elements.evalFrame.contentWindow.postMessage({ type: "eval", value: command });
  } while ("timeout" == await timeout(receiveMessage("evaluating"), 100))

  const result = await receiveMessage("result");
  if (result.status == "ok") {
    Elements.stateDisplay.textContent = "Done.";
    Elements.timeDisplay.textContent = `${result.dtMs.toFixed(3)} ms`;
    Elements.resultDisplay.textContent = result.value;
  } else if (result.status == "error") {
    Elements.stateDisplay.textContent = "Evaluation error!";
    Elements.timeDisplay.textContent = "";
    Elements.resultDisplay.textContent = "";
  }
}

function displayLog(log, isError) {
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

function receiveMessage(type) {
  return new Promise(function (resolve) {
    window.addEventListener("message", function messageListener(e) {
      if (e.data.type == type) {
        window.removeEventListener("message", messageListener);
        resolve(e.data.value);
      }
    });
  });
}

function timeout(promise, timeout) {
  return Promise.race([
    promise,
    new Promise(function (resolve) {
      setTimeout(function () { resolve("timeout"); }, timeout);
    })
  ]);
}

setup();
