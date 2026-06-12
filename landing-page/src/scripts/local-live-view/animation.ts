import { $assigns, $flashKeys, $step } from "./store";

function isLineVisible(el: HTMLElement): boolean {
  const { top } = el.getBoundingClientRect();
  return top >= 0 && top < window.innerHeight;
}

function isElementInView(el: HTMLElement): boolean {
  const { top, bottom } = el.getBoundingClientRect();
  return top < window.innerHeight && bottom > 0;
}

let flashTimer: ReturnType<typeof setTimeout> | null = null;

export function applyAssignsWithFlash(newAssigns: Record<string, unknown>) {
  const prev = $assigns.get();
  const changed = new Set(
    Object.keys(newAssigns).filter((k) => prev[k] !== newAssigns[k]),
  );
  $assigns.set(newAssigns);
  if (changed.size > 0) {
    if (flashTimer !== null) clearTimeout(flashTimer);
    $flashKeys.set(changed);
    flashTimer = setTimeout(() => {
      $flashKeys.set(new Set());
      flashTimer = null;
    }, 800);
  }
}

export function setCodeHighlight(
  containerId: string,
  activeBlock: string | null,
) {
  const container = document.getElementById(containerId);
  if (!container) return;

  const firstLine = container.querySelector<HTMLElement>(`.line[data-block="${activeBlock}"]`);
  if (firstLine && !isLineVisible(firstLine)) {
    firstLine.scrollIntoView({ behavior: "smooth" });
  }
  container.querySelectorAll<HTMLElement>(".line").forEach((line) => {
    const b = line.dataset.block;
    if (!activeBlock) {
      line.classList.remove("hl", "dim");
    } else if (b === activeBlock) {
      line.classList.add("hl");
      line.classList.remove("dim");
    } else {
      line.classList.add("dim");
      line.classList.remove("hl");
    }
  });
}

export function highlightHtml(key: string, state: boolean) {
  const el = document.querySelector<HTMLElement>(`[data-value="${key}"]`);
  if (!el) return;

  const container = el.closest<HTMLElement>("[data-pop-view]") ?? el;
  const needsScroll = state && !isElementInView(container);
  container.classList.toggle(`hl-${key}`, state);
  if (needsScroll) container.scrollIntoView({ behavior: "smooth" });
}

export type StepDef<T> = {
  delay: number;
  onEnter?: (ctx: T) => void;
};

export type AnimationOptions<T> = {
  completeDelay: number;
  onComplete?: (ctx: T) => void;
  onCancel?: (ctx: T) => void;
};

export function createAnimation<T = unknown>(
  steps: StepDef<T>[],
  options: AnimationOptions<T>,
): { run: (ctx: T) => void; cancel: (ctx: T) => void } {
  let timers: ReturnType<typeof setTimeout>[] = [];
  let runningCtx: T | null = null;

  function cancelInternal() {
    if (timers.length === 0) return;
    timers.forEach(clearTimeout);
    timers = [];
    if (runningCtx !== null) options.onCancel?.(runningCtx);
    runningCtx = null;
  }

  function run(ctx: T) {
    cancelInternal();
    runningCtx = ctx;
    steps.forEach(({ delay, onEnter }, i) => {
      timers.push(
        setTimeout(() => {
          $step.set(i);
          onEnter?.(ctx);
        }, delay),
      );
    });
    timers.push(
      setTimeout(() => {
        timers = [];
        runningCtx = null;
        options.onComplete?.(ctx);
        $step.set(null);
      }, options.completeDelay),
    );
  }

  return { run, cancel: cancelInternal };
}
