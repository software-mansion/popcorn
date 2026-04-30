import createModule from "../assets/beam.mjs";

import { boot, send } from "./beam";
import { readMainEvent, toMain } from "./events";
import type { EmscriptenModule } from "./types";
import { check, unreachable } from "./utils";

let instance: EmscriptenModule | null = null;

self.onmessage = async (event: MessageEvent<unknown>) => {
  const data = readMainEvent(event.data);
  check(data !== null);

  switch (data.type) {
    case "popcorn:boot": {
      check(instance === null);

      const result = await boot({
        assetsUrl: data.payload.assetsUrl,
        extraArgs: [],
        searchPaths: [],
        createModule,
        emit: toMain,
      });
      if (!result.ok) {
        toMain({
          type: "popcorn:boot-fail",
          payload: result.error.serialize(),
        });
        return;
      }

      instance = result.data;
      toMain({ type: "popcorn:boot-end", payload: {} });
      return;
    }
    case "popcorn:send": {
      const result = send(instance, data.payload.command);
      if (!result.ok) {
        toMain({
          type: "popcorn:send-fail",
          payload: result.error.serialize(),
        });
      }
      return;
    }
    default:
      return unreachable();
  }
};
