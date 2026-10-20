const native = new URL(self.location.href).searchParams.get("native");
if (native === "boot") throw new Error("native boot failure");

// Default mode aborts during boot without sending a completion message.
self.onmessage = (event) => {
  const data = event.data;
  if (native === "runtime") {
    if (data.type === "popcorn:boot") {
      self.postMessage({ type: "popcorn:boot-end", payload: {} });
      return;
    }
    throw new Error("native runtime failure");
  }
  if (data && data.type === "popcorn:boot") {
    self.postMessage({
      type: "otp:error",
      payload: { kind: "abort", data: "boom" },
    });
  }
};
