let ttySize;

self.onmessage = (event) => {
  if (event.data?.type === "popcorn:boot") {
    ttySize = event.data.payload.ttySize;
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
    self.postMessage({
      type: "otp:message",
      payload: command === 2 ? { ttySize } : { command },
    });
  }
};

function emit(type, bytes) {
  self.postMessage({ type, payload: new Uint8Array(bytes) });
}
