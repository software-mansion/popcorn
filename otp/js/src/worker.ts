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
        extraArgs: data.payload.extraArgs,
        searchPaths: data.payload.searchPaths,
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
      break;
    }
    case "popcorn:send": {
      const result = send(instance, data.payload.message);
      toMain({
        type: "popcorn:send-end",
        payload: {
          id: data.payload.id,
          result: result.ok
            ? { ok: true, data: null }
            : { ok: false, error: result.error.serialize() },
        },
      });
      break;
    }
    default:
      unreachable();
  }
};
