import init from "../wasm/AtomVM.mjs";

const MESSAGES = {
  INIT: "fission-init",
  CALL: "fission-call",
  CAST: "fission-cast",
  CALL_ACK: "fission-callAck",
  STDOUT: "fission-stdout",
  STDERR: "fission-stderr",
  HEARTBEAT: "fission-heartbeat",
};

const HEARTBEAT_INTERVAL_MS = 500;

let Module = null;

export async function initVm() {
  const bundleName = document.querySelector('meta[name="bundle-name"]').content;

  Module = await init({
    arguments: ["../" + bundleName],
    onRuntimeInitialized: onVmInit,
    print(text) {
      send(MESSAGES.STDOUT, text);
    },
    printErr(text) {
      send(MESSAGES.STDERR, text);
    },
  });
}

async function onVmInit() {
  // TODO: implement full message receive from Elixir
  await timeout(300);
  setInterval(() => send(MESSAGES.HEARTBEAT, null), HEARTBEAT_INTERVAL_MS);

  window.addEventListener("message", async ({ data }) => {
    const type = data.type;

    if (type === MESSAGES.CALL) {
      const { requestId, process, action, args } = data.value;
      send(MESSAGES.CALL_ACK, { requestId });

      try {
        const result = await Module.call(process, serialize({ action, args }));
        send(MESSAGES.CALL, { requestId, data: deserialize(result) });
      } catch (error) {
        send(MESSAGES.CALL, { requestId, error: deserialize(error) });
      }
    } else if (type.startsWith("fission")) {
      `Iframe: received unhandled fission event: ${JSON.stringify(data, null, 4)}`;
    }
  });
  send(MESSAGES.INIT, "main");
}

function send(type, data) {
  window.parent.postMessage({ type, value: data });
}

function timeout(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

function serialize({ action, args }) {
  // TODO: serialize args
  return [action, args].join(":");
}

function deserialize(message) {
  return message;
}
