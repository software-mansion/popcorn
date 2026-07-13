// Test double for the VM worker. On boot it emits an otp:error and then goes
// silent — never sending popcorn:boot-end or popcorn:boot-fail — to reproduce a
// runtime fault that occurs mid-boot. This exercises Popcorn's path where the
// boot must be failed by the error itself rather than stalling to its timeout.
self.onmessage = (event) => {
  const data = event.data;
  if (data && data.type === "popcorn:boot") {
    self.postMessage({
      type: "otp:error",
      payload: { kind: "abort", data: "boom" },
    });
  }
};
