let ttySize;
let noshell;

self.onmessage = (event) => {
  if (event.data?.type === "popcorn:boot") {
    ttySize = event.data.payload.ttySize;
    noshell = event.data.payload.noshell;
    emit("otp:stdout", [0xf0, 0x9f]);
    emit("otp:stderr", [0xf0, 0x9f, 0x9a]);
    emit("otp:stdout", [0x91, 0xa9, 0xe2, 0x80]);
    emit("otp:stdout", [0x8d, 0xf0, 0x9f, 0x9a, 0x80]);
    emit("otp:stderr", [0x80]);
    self.postMessage({ type: "popcorn:boot-end", payload: {} });
    return;
  }

  if (event.data?.type === "popcorn:stdin") {
    const command = event.data.payload.chunk[0];
    if (command === 0) emit("otp:stdout", [0xf0, 0x9f]);
    if (command === 1) {
      emit("otp:stdout", [
        0xf0, 0x9f, 0x91, 0xa9, 0xe2, 0x80, 0x8d, 0xf0, 0x9f, 0x9a, 0x80,
      ]);
    }
    self.postMessage({ type: "otp:stdin-consumed", payload: 1 });
    emit(
      "otp:message",
      encode(command === 2 ? { ttySize, noshell } : { command }),
    );
  }
};

function emit(type, bytes) {
  self.postMessage({ type, payload: new Uint8Array(bytes) });
}

// Like wasm.erl, which the page decodes
function encode(value) {
  if (typeof value === "boolean") return [value ? 2 : 1];
  if (typeof value === "number") {
    const number = new DataView(new ArrayBuffer(8));
    number.setFloat64(0, value);
    return [3, ...new Uint8Array(number.buffer)];
  }
  const entries = Object.entries(value);
  const bytes = [9, ...uint32(entries.length)];
  for (const [key, item] of entries) {
    const name = new TextEncoder().encode(key);
    bytes.push(...uint32(name.length), ...name, ...encode(item));
  }
  return bytes;
}

function uint32(value) {
  return [value >>> 24, (value >>> 16) & 255, (value >>> 8) & 255, value & 255];
}
