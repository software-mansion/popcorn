import createModule from "../assets/beam.mjs";

import { boot } from "./beam";
import { isPopcornEvent, toMain } from "./events";
import type { EmscriptenModule } from "./types";
import { check } from "./utils";

let instance: EmscriptenModule | null = null;

self.onmessage = async (event: MessageEvent<unknown>) => {
  check(isPopcornEvent(event), "beam:bad-message");
  check(instance === null, "beam:double-init");

  const result = await boot({
    assetsUrl: event.data.payload.assetsUrl,
    extraArgs: [],
    searchPaths: [],
    createModule,
    emit: () => {
      // TODO: pass it to main context. For now, swallow all events.
    },
  });
  if (result.ok) {
    instance = result.module;
    toMain({ type: "popcorn:boot-end", payload: null });
  } else {
    toMain({
      type: "popcorn:boot-fail",
      payload: result.error.serialize(),
    });
  }
};
