var Module = {
  arguments: ["iex_wasm.avm"],
  print(text) {
    console.log(text);
  },
  printErr(text) {
    console.error(text);
  },
};

const Elements = {
  get evalButton() {
    return document.getElementById("eval");
  },
  get codeInput() {
    return document.getElementById("code");
  },
  get timeDisplay() {
    return document.getElementById("time");
  },
  get resultDisplay() {
    return document.getElementById("result");
  },
};

async function profile(fn) {
  const t = performance.now();
  const result = await fn();
  const dtMs = performance.now() - t;

  return { result, dtMs };
}

async function evalCode() {
  const code = Elements.codeInput.value;

  console.log("Evaluating");
  const { result, dtMs } = await profile(async () => Module.call("main", code));
  console.log(`Took ${dtMs} ms, result: ${result}`);

  Elements.timeDisplay.textContent = `${dtMs.toFixed(3)} ms`;
  Elements.resultDisplay.textContent = result;
}

Elements.evalButton.onclick = () => evalCode();
Elements.codeInput.addEventListener("keydown", (event) => {
  const cmdEnter = event.key === "Enter" && (event.metaKey || event.ctrlKey);
  if (cmdEnter) {
    evalCode();
  }
});
