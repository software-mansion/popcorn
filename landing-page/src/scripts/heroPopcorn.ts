import type { PopcornRuntime } from "./popcornScene";

const UNLOAD_DELAY = 3_000;

export function mountHeroPopcorn(container: HTMLElement) {
  const views = Array.from(
    container.querySelectorAll<HTMLElement>(".popcorn-view"),
  );
  assert(views.length === 2, "Hero requires two popcorn views");

  let runtime: PopcornRuntime | null = null;
  let load: Promise<void> | null = null;
  let loadController: AbortController | null = null;
  let idleCallback = 0;
  let unloadTimer = 0;
  let disposed = false;
  const visible = views.map(() => false);

  const observer = new IntersectionObserver(
    (entries) => {
      for (const entry of entries) {
        const index = views.indexOf(entry.target as HTMLElement);
        visible[index] = entry.isIntersecting;
      }

      runtime?.setVisibility(visible);
      if (visible.some(Boolean)) {
        clearTimeout(unloadTimer);
        unloadTimer = 0;
        scheduleLoad();
      } else {
        cancelScheduledLoad();
        scheduleUnload();
      }
    },
    { threshold: 0 },
  );

  for (const view of views) observer.observe(view);
  document.addEventListener("visibilitychange", onVisibilityChange);
  window.addEventListener("pagehide", dispose, { once: true });
  document.addEventListener("astro:before-swap", dispose, { once: true });

  function scheduleLoad() {
    if (
      runtime !== null ||
      load !== null ||
      idleCallback !== 0 ||
      disposed ||
      !shouldLoadModel()
    )
      return;
    idleCallback = requestIdle(() => {
      idleCallback = 0;
      if (visible.some(Boolean)) loadRuntime();
    });
  }

  function loadRuntime() {
    loadController = new AbortController();
    const signal = loadController.signal;
    load = import("./popcornScene")
      .then(({ createPopcornRuntime }) => createPopcornRuntime(views, signal))
      .then((created) => {
        if (disposed || signal.aborted) {
          created.dispose();
          return;
        }
        runtime = created;
        runtime.setVisibility(visible);
        runtime.setPageVisible(!document.hidden);
        if (!visible.some(Boolean)) scheduleUnload();
      })
      .catch((error: unknown) => {
        if (signal.aborted) return;
        console.error("Unable to load the hero popcorn model", error);
      })
      .finally(() => {
        load = null;
        loadController = null;
      });
  }

  function scheduleUnload() {
    if ((runtime === null && load === null) || unloadTimer !== 0 || disposed)
      return;
    unloadTimer = window.setTimeout(() => {
      unloadTimer = 0;
      unloadRuntime();
    }, UNLOAD_DELAY);
  }

  function unloadRuntime() {
    loadController?.abort();
    try {
      runtime?.dispose();
    } finally {
      runtime = null;
    }
  }

  function cancelScheduledLoad() {
    if (idleCallback === 0) return;
    cancelIdle(idleCallback);
    idleCallback = 0;
  }

  function onVisibilityChange() {
    runtime?.setPageVisible(!document.hidden);
  }

  function dispose() {
    if (disposed) return;
    disposed = true;
    observer.disconnect();
    document.removeEventListener("visibilitychange", onVisibilityChange);
    clearTimeout(unloadTimer);
    cancelScheduledLoad();
    unloadRuntime();
  }

  return { dispose };
}

function requestIdle(callback: () => void) {
  if ("requestIdleCallback" in window) {
    return window.requestIdleCallback(callback, { timeout: 1_500 });
  }
  return window.setTimeout(callback, 1);
}

function cancelIdle(id: number) {
  if ("cancelIdleCallback" in window) window.cancelIdleCallback(id);
  else clearTimeout(id);
}

function shouldLoadModel() {
  if (matchMedia("(prefers-reduced-motion: reduce)").matches) return false;
  const connection =
    (navigator as Navigator & { connection?: HeroNetworkInformation })
      .connection ?? null;
  return (
    connection?.saveData !== true &&
    !["slow-2g", "2g"].includes(connection?.effectiveType ?? "")
  );
}

type HeroNetworkInformation = {
  effectiveType?: string;
  saveData?: boolean;
};

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}
