var Module = {
  arguments: ["app.avm"],
  onRuntimeInitialized: init,
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

async function init() {
  // Wait until the receive loop starts within AVM
  await new Promise(r => setTimeout(r, 100));
  console.log("init")
  window.addEventListener("message", async (e) => {
    if (e.data.type == "eval") {
      window.parent.postMessage({ type: "evaluating" });
      const result = await evalCode(e.data.value);
      window.parent.postMessage({ type: "result", value: result });
      if (result.status == "error") {
        window.location.reload();
      }
    } else {
      console.error("Invalid message", e.data);
    }
  });
}