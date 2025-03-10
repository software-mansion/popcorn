var Module = {
  arguments: ["app.avm"],
  print(text) {
    console.log(text);
    window.parent.postMessage({ type: "log", value: text });
  },
  printErr(text) {
    console.error(text);
    window.parent.postMessage({ type: "log_error", value: text });
  },
};

async function profile(fn) {
  const t = performance.now();
  const result = await fn();
  const dtMs = performance.now() - t;

  return { result, dtMs };
}

async function evalCode(command) {
  try {
    const { result, dtMs } = await profile(async () =>
      Module.call("main", command)
    );
    console.log(`Took ${dtMs} ms, result: ${result}`);
    return { status: "ok", dtMs: dtMs, value: result };
  } catch (e) {
    console.error(e);
    return { status: "error", value: e };
  }
}
window.addEventListener("message", async (e) => {
  if (e.data.type == "eval") {
    const result = await evalCode(e.data.value);
    window.parent.postMessage({ type: "result", value: result });
    if (result.status == "error") {
      window.location.reload();
    }
  } else {
    console.error("Invalid message", e.data);
  }
});