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
        manifestUrl: data.payload.manifestUrl,
        emulatorArgs: data.payload.emulatorArgs,
        extraArgs: data.payload.extraArgs,
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
    case "popcorn:run-js-reply": {
      // ignore the `send()` result, process could've died
      send(instance, data.payload.message);
      break;
    }
    default:
      unreachable();
  }
};
