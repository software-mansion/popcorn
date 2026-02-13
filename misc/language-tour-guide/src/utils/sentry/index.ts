import * as Sentry from "@sentry/react";
import { useEditorsStore } from "../../store/editors";

export function initSentry() {
  console.log(import.meta.env);

  if (
    !import.meta.env.VITE_SENTRY_DSN ||
    import.meta.env.VITE_PLAYWRIGHT_TEST
  ) {
    return;
  }

  Sentry.init({
    dsn: import.meta.env.VITE_SENTRY_DSN,
    release: import.meta.env.VITE_APP_VERSION,
    sendDefaultPii: true,
    environment: import.meta.env.VITE_MODE || "development",
    replaysOnErrorSampleRate: 1.0,
    replaysSessionSampleRate: 0.2,
    integrations: (integrations) => [
      ...integrations.filter((integration) => integration.name !== "Dedupe"),
      Sentry.browserSessionIntegration(),
      Sentry.replayIntegration({
        maskAllText: false,
        maskAllInputs: false
      })
    ]
  });
}

export function captureCodeException(error: string, code: string) {
  Sentry.captureMessage("code-exception", {
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
  reason: { source: "heartbeat_lost" | "atomvm"; category: string },
  stdout: string,
  stderr: string
) {
  const { editors, editorOrder } = useEditorsStore.getState();

  const failedEditor = [...editors.values()].find(
    (editor) => editor.executionState === "running"
  );

  const code = editorOrder
    .map((id) => {
      const editor = editors.get(id);
      return editor ? `# Editor: ${id}\n${editor.code}` : null;
    })
    .join("\n\n");

  const context = {
    tags: {
      source: reason.source,
      category: reason.category
    },
    extra: {
      code,
      stdout,
      stderr,
      failedEditorId: failedEditor?.id || null
    }
  };

  if (reason.source === "heartbeat_lost") {
    Sentry.captureMessage(reason.category, context);
    return;
  }

  const exception = new Error(`Iframe reload: ${reason.category}`);
  exception.name = reason.category;

  Sentry.captureException(exception, context);
}

type ReloadReason = "heartbeat_lost" | "unknown";

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
