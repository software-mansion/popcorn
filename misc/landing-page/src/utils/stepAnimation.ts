type StepVisibilityOptions = {
  sectionSelector: string;
  stepSelector: string;
  onUpdate: (
    steps: HTMLElement,
    nextStep: HTMLElement,
    scale: number,
    extra: {
      currentStep: number;
      stepRect: DOMRect;
      nextStepRect: DOMRect;
      distance: number;
      isCurrentStepIsInBottomHalfOfViewport: boolean;
    },
  ) => void;
  threshold?: number;
  duration?: number;
};

export function setupStepVisibility({
  sectionSelector,
  stepSelector,
  onUpdate,
  threshold = 50,
  duration = 500,
}: StepVisibilityOptions) {
  const section = document.querySelector(sectionSelector) as HTMLElement | null;

  if (!section) return;

  section.style.gap = `${duration * 1.7}px`;

  const steps = Array.from(
    document.querySelectorAll<HTMLElement>(stepSelector),
  );

  if (steps.length < 2) return;

  let currentStep = 0;
  let isInViewport = false;
  let ticking = false;

  function updateVisibility() {
    const step = steps[currentStep];
    const stepRect = step.getBoundingClientRect();
    const nextStep = steps[currentStep + 1];
    const nextStepRect = nextStep?.getBoundingClientRect();

    if (!nextStep || !nextStepRect) return;

    const distance = nextStepRect.top - stepRect.top;
    const scale = Math.max(Math.min((distance - threshold) / duration, 1), 0);

    // const scale = 0.9 + 0.1 * opacity;

    const isCurrentStepIsInBottomHalfOfViewport =
      stepRect.top + stepRect.height / 2 - threshold * 2 >
      window.innerHeight / 2;

    onUpdate(step, nextStep, scale, {
      currentStep,
      stepRect,
      nextStepRect,
      distance,
      isCurrentStepIsInBottomHalfOfViewport,
    });

    if (Math.abs(distance) < threshold && currentStep < steps.length - 2) {
      currentStep += 1;
    }
    if (isCurrentStepIsInBottomHalfOfViewport && currentStep > 0) {
      currentStep -= 1;
    }
  }

  function handleScroll() {
    if (!ticking && isInViewport) {
      requestAnimationFrame(() => {
        updateVisibility();
        ticking = false;
      });
      ticking = true;
    }
  }

  const observer = new IntersectionObserver(
    (entries) => {
      entries.forEach((entry) => {
        isInViewport = entry.isIntersecting;
      });
    },
    { threshold: 0 },
  );

  observer.observe(section);

  // Initial visibility update
  steps.forEach((step) => {
    updateVisibility();
  });

  window.addEventListener("scroll", handleScroll);
}
