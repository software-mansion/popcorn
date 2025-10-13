import * as Sentry from "@sentry/react";
import { useCodeEditorStore } from "../../store/codeEditor";

export function initSentry() {
  Sentry.init({
    dsn: import.meta.env.VITE_SENTRY_DSN,
    sendDefaultPii: true,
    environment: import.meta.env.MODE
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
