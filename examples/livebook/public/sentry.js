// Reports the example's failures to Sentry, once docker/livebook.Dockerfile has
// set a DSN in index.html: the shell page's JavaScript errors, Livebook's VM's
// crashes, and the VMs that boot or don't.
//
// Notebooks run their code in VMs of their own, and their outputs' JavaScript
// in frames of their own, so their errors aren't the shell page's. A notebook's
// VM can crash because of its code, so only its boot failures are reported,
// before it runs any. The reports carry no code, output or VMs' messages:
// Sentry gets no breadcrumbs, as the console has the VMs' output and fetch()
// the notebooks' HTTP requests, and no pages' queries, like /import?url=…

const SDK = {
  src: "https://browser.sentry-cdn.com/10.74.0/bundle.min.js",
  integrity: "sha384-ekhFYbUiD5vrs1pT5Kty50wAgPsq9eoEDo+hUVl7VahsXkikMcPAq0W0HCoTjGTY",
};

// The VMs' workers, whose errors the browser passes to the page without their
// stacks when a VM crashes. The crashes are reported by reportVmCrash instead.
const VM_WORKERS = /\/out\/(beam\.emu|worker)\.mjs/;

const NAMES = { livebook: "Livebook's VM", notebook: "A notebook's VM" };

const setting = (name) => document.querySelector(`meta[name="sentry-${name}"]`)?.content || undefined;

let sentry = null;

export function startSentry() {
  const dsn = setting("dsn");
  if (!dsn) return;
  sentry = new Promise((resolve, reject) => {
    const script = Object.assign(document.createElement("script"), {
      src: SDK.src,
      integrity: SDK.integrity,
      crossOrigin: "anonymous",
    });
    script.addEventListener("load", () => resolve(init(window.Sentry, dsn)));
    script.addEventListener("error", () => reject(new Error("Sentry's SDK didn't load")));
    document.head.append(script);
  }).catch((error) => console.warn(`[shell] ${error.message}`));
}

function init(Sentry, dsn) {
  Sentry.init({
    dsn,
    environment: setting("environment") || "development",
    release: setting("release"),
    sendDefaultPii: false,
    integrations: (defaults) => defaults.filter(({ name }) => name !== "Breadcrumbs"),
    initialScope: { tags: { app: "livebook" } },
    beforeSend: scrub,
  });
  return Sentry;
}

function scrub(event, hint) {
  const frames = (event.exception?.values ?? []).flatMap(({ stacktrace }) => stacktrace?.frames ?? []);
  if (frames.some(({ filename }) => VM_WORKERS.test(filename ?? ""))) return null;
  // The calls to a VM that has exited, which reportVmCrash reports
  if (hint?.originalException?.t === "vm:exited") return null;
  for (const frame of frames) if (frame.filename) frame.filename = withoutQuery(frame.filename);
  if (event.request) {
    if (event.request.url) event.request.url = withoutQuery(event.request.url);
    delete event.request.headers?.Referer;
  }
  return event;
}

function withoutQuery(url) {
  return url.replace(/[?#].*/, "");
}

// A VM's boot, against which to count its failures, with how long it took
export function reportBoot(vm, ms) {
  report((Sentry) =>
    Sentry.captureMessage(`${NAMES[vm]} booted`, {
      level: "info",
      tags: { vm },
      contexts: { boot: { ms: Math.round(ms) } },
    }),
  );
}

// A VM's exit after it booted, from Popcorn's onError, unless it's a normal one
export function reportVmCrash(vm, { kind, data }) {
  if (kind === "exit" && data === 0) return;
  const what = { abort: "aborted", error: "exited with an error", exit: `exited with status ${data}` }[kind];
  // An error's message is the VM's, whose details can hold its processes' data
  const reason = kind === "abort" ? data : kind === "error" ? data.split("(")[0].trim().slice(0, 100) : undefined;
  report((Sentry) =>
    Sentry.captureMessage(`${NAMES[vm]} ${what}`, {
      level: "fatal",
      tags: { vm, exit: kind },
      contexts: { vm: { kind, reason, status: kind === "exit" ? data : undefined } },
    }),
  );
}

// A VM's failure to boot, a PopcornError from Popcorn.init, by its tag
export function reportBootFailure(vm, error) {
  const tag = error?.t ?? error?.name ?? "unknown";
  const details = tag === "timeout" ? error.data : tag === "vm:exited" ? { reason: error.data.reason } : {};
  report((Sentry) =>
    Sentry.captureMessage(`${NAMES[vm]} didn't boot: ${tag}`, {
      level: "error",
      tags: { vm, boot: tag },
      contexts: { boot: details },
    }),
  );
}

function report(send) {
  sentry?.then((Sentry) => Sentry && send(Sentry));
}
