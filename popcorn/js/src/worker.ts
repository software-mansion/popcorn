import createModule from "../assets/beam.mjs";

import { start, type Beam } from "./beam";
import { readMainEvent, toMain } from "./events";
import { check, unreachable } from "./utils";

let instance: Beam | null = null;

self.onmessage = async (event: MessageEvent<unknown>) => {
  const data = readMainEvent(event.data);

  switch (data.type) {
    case "popcorn:boot": {
      check(instance === null);

      instance = start({
        manifestUrl: data.payload.manifestUrl,
        emulatorArgs: data.payload.emulatorArgs,
        extraArgs: data.payload.extraArgs,
        env: data.payload.env,
        ttySize: data.payload.ttySize,
        createModule,
        emit: toMain,
      });
      const result = await instance.boot;
      if (!result.ok) {
        toMain({
          type: "popcorn:boot-fail",
          payload: result.error.serialize(),
        });
        return;
      }

      toMain({ type: "popcorn:boot-end", payload: {} });
      break;
    }
    case "popcorn:send": {
      check(instance !== null);
      const result = instance.send(data.payload.message);
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
      check(instance !== null);
      instance.send(data.payload.message);
      break;
    }
    case "popcorn:stdin": {
      check(instance !== null);
      instance.writeStdin(data.payload.chunk);
      break;
    }
    case "popcorn:tty-resize": {
      check(instance !== null);
      instance.resizeTty(data.payload.columns, data.payload.rows);
      break;
    }
    default:
      unreachable();
  }
};
