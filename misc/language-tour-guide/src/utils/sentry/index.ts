import * as Sentry from "@sentry/react";
import { useCodeEditorStore } from "../../store/codeEditor";

export function initSentry() {
  Sentry.init({
    dsn: import.meta.env.VITE_SENTRY_DSN,
    sendDefaultPii: true,
    environment: import.meta.env.MODE,
    replaysOnErrorSampleRate: 1.0,
    replaysSessionSampleRate: 0.2,
    integrations: (integrations) => [
      ...integrations.filter((integration) => integration.name !== "Dedupe"),
      Sentry.replayIntegration({
        maskAllText: false,
        maskAllInputs: false
      })
    ]
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

export function captureReloadIframe(
  reason: { source: string; category: string },
  stdout: string,
  stderr: string
) {
  const code = useCodeEditorStore.getState().code;

  Sentry.captureMessage(reason.category, {
    level: "error",
    tags: {
      source: reason.source,
      category: reason.category
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
  onCrash: (reason: ReloadReason) => void;
};

export function createLogSink(): LogSink {
  const stdout: string[] = [];
  const stderr: string[] = [];

  return {
    onStdout: (text: string) => {
      console.log("Popcorn stdout:", text);
      stdout.push(text);
    },
    onStderr: (text: string) => {
      console.error("Popcorn stderr:", text);
      stderr.push(text);
    },
    onCrash: (reason: ReloadReason) => {
      const stdoutJoined = stdout.join("\n");
      const stderrJoined = stderr.join("\n");

      if (reason === "heartbeat_lost") {
        captureReloadIframe(
          { source: "heartbeat_lost", category: "popcorn-reload" },
          stdoutJoined,
          stderrJoined
        );
      } else {
        captureReloadIframe(
          { source: "atomvm", category: "atomvm-crash" },
          stdoutJoined,
          stderrJoined
        );
      }

      // preserve ref to arrays, could also use stable object and change properties
      stdout.length = 0;
      stderr.length = 0;
    }
  };
}
export type ReloadReason = "heartbeat_lost" | "unknown";

export function wrapPopcornReloadIframe(
  popcornInstance: any,
  customReloadCallback: (reason: ReloadReason) => void
) {
  const originalReloadIframe = popcornInstance.__proto__._reloadIframe;

  // TODO: not overwriting prototype method
  popcornInstance.__proto__._reloadIframe = function (reason: ReloadReason) {
    originalReloadIframe.call(this, reason);

    customReloadCallback(reason);
  };
}
