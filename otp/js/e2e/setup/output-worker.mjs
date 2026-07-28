self.onmessage = (event) => {
  if (event.data?.type !== "popcorn:boot") return;

  self.postMessage({
    type: "otp:stdout",
    payload: new Uint8Array([0xf0, 0x9f]),
  });
  self.postMessage({
    type: "otp:stderr",
    payload: new Uint8Array([0xf0, 0x9f, 0x9a]),
  });
  self.postMessage({
    type: "otp:stdout",
    payload: new Uint8Array([0x91, 0xa9, 0xe2, 0x80]),
  });
  self.postMessage({
    type: "otp:stdout",
    payload: new Uint8Array([0x8d, 0xf0, 0x9f, 0x9a, 0x80]),
  });
  self.postMessage({
    type: "otp:stderr",
    payload: new Uint8Array([0x80]),
  });
  self.postMessage({ type: "popcorn:boot-end", payload: {} });
};
