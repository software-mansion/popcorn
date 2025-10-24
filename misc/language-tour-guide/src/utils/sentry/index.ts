import * as Sentry from "@sentry/react";
import { useCodeEditorStore } from "../../store/codeEditor";

export function initSentry() {
  Sentry.init({
    dsn: import.meta.env.VITE_SENTRY_DSN,
    sendDefaultPii: true,
    environment: import.meta.env.MODE,
    integrations: (integrations) =>
      integrations.filter((integration) => integration.name !== "Dedupe")
  });
}

export function captureCodeException(error: string, code: string) {
  Sentry.captureMessage("code-exception", {
    level: "error",
    tags: {
      source: "code",
      category: "code-exception"
    },
    extra: {
      code,
      error
    }
  });
}

export function captureAtomVmCrash(stdout: string, stderr: string) {
  const code = useCodeEditorStore.getState().code;
  console.log("Capturing AtomVM crash with code:", code);

  Sentry.captureMessage("atomvm-crash", {
    level: "error",
    tags: {
      source: "atomvm",
      category: "atomvm-crash"
    },
    extra: {
      code,
      stdout,
      stderr
    }
  });
}

export type LogSink = {
  onStdout: (text: string) => void;
  onStderr: (text: string) => void;
  onCrash: () => void;
};
export function createLogSink() {
  const stdout: string[] = [];
  const stderr: string[] = [];

  return {
    onStdout: (text: string) => {
      stdout.push(text);
    },
    onStderr: (text: string) => {
      stderr.push(text);
    },
    onCrash: () => {
      captureAtomVmCrash(stdout.join("\n"), stderr.join("\n"));
      // preserve ref to arrays, could also use stable object and change properties
      stdout.length = 0;
      stderr.length = 0;
    }
  };
}

export function wrapPopcornReloadIframe(
  popcornInstance: any,
  customReloadCallback: () => void
) {
  const originalReloadIframe = popcornInstance.__proto__._reloadIframe;

  // TODO: not overwriting prototype method
  popcornInstance.__proto__._reloadIframe = function () {
    originalReloadIframe.call(this);

    customReloadCallback();
  };
}
