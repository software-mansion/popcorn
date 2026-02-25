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
  onLeave?: (step: HTMLElement, index: number, currentStep: number) => void;
  threshold?: number;
  duration?: number;
};

const LERP_FACTOR = 0.16;

export function setupStepVisibility({
  sectionSelector,
  stepSelector,
  onUpdate,
  onLeave,
  threshold = 80,
  duration = 500,
}: StepVisibilityOptions) {
  const section = document.querySelector(sectionSelector) as HTMLElement | null;

  if (!section) return;

  section.style.gap = `${duration * 1.7}px`;

  const spacer = document.createElement("div");
  spacer.style.height = "0px";
  spacer.setAttribute("aria-hidden", "true");
  section.appendChild(spacer);

  const steps = Array.from(
    document.querySelectorAll<HTMLElement>(stepSelector),
  );

  if (steps.length < 2) return;

  let currentStep = 0;
  let prevCurrentStep = 0;
  let isInViewport = false;
  let smoothedScale = 1;
  let ticking = false;

  function updateVisibility() {
    if (currentStep !== prevCurrentStep) {
      smoothedScale = currentStep > prevCurrentStep ? 1 : 0;
      prevCurrentStep = currentStep;
    }

    const step = steps[currentStep];
    const stepRect = step.getBoundingClientRect();
    const nextStep = steps[currentStep + 1];
    const nextStepRect = nextStep?.getBoundingClientRect();

    if (!nextStep || !nextStepRect) return;

    const distance = nextStepRect.top - stepRect.top;
    const targetScale = Math.max(
      Math.min((distance - threshold) / duration, 1),
      0,
    );

    smoothedScale += (targetScale - smoothedScale) * LERP_FACTOR;

    const isCurrentStepIsInBottomHalfOfViewport =
      stepRect.top + stepRect.height / 2 - threshold * 2 >
      window.innerHeight / 2;

    if (onLeave) {
      steps.forEach((s, i) => {
        if (i !== currentStep && i !== currentStep + 1)
          onLeave(s, i, currentStep);
      });
    }

    onUpdate(step, nextStep, smoothedScale, {
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
    ([entry]) => {
      isInViewport = entry.isIntersecting;
    },
    { threshold: 0 },
  );

  observer.observe(section);

  steps.forEach(() => {
    updateVisibility();
  });

  window.addEventListener("scroll", handleScroll);
}
