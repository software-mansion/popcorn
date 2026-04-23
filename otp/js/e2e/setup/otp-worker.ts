import { boot, type BeamBootOptions } from "@swmansion/popcorn-otp/beam";

type BootRequest = {
  type: "boot";
} & BeamBootOptions;

function isBootRequest(
  event: MessageEvent<unknown>,
): event is MessageEvent<BootRequest> {
  const isObject = typeof event.data === "object" && event.data !== null;
  const hasTypeKey = isObject && hasOwn(event.data, "type");
  const isBootMessage = hasTypeKey && event.data?.type === "boot";
  return isBootMessage;
}

self.addEventListener("message", async (event: MessageEvent<unknown>) => {
  if (!isBootRequest(event)) {
    return;
  }

  const { assetsUrl, searchPaths, extraArgs } = event.data;

  const result = await boot({
    assetsUrl,
    searchPaths,
    extraArgs,
  });
  if (result.ok) {
    self.postMessage({
      type: "boot:ok",
      data: { args: result.module.arguments },
    });
  } else {
    self.postMessage({
      type: "boot:error",
      data: {
        name: result.error.name,
        message: result.error.message,
      },
    });
  }
});

function hasOwn<K extends string>(
  obj: object,
  key: K,
): obj is Record<K, unknown> {
  return Object.hasOwn(obj, key);
}
