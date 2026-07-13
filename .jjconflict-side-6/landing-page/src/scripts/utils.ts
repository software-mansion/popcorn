export async function onPageLoad(fn: () => Promise<void>) {
  document.addEventListener("astro:page-load", fn);
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", fn);
  } else {
    await fn();
  }
}

type ContainerFn = (node: HTMLElement) => void;
export function preventFocus(node: HTMLElement, fn: ContainerFn): void {
  const original = node.focus;
  const restoreOriginal = () => {
    node.focus = original;
  };
  node.focus = () => {};

  try {
    fn(node);
  } finally {
    setTimeout(restoreOriginal, 0);
  }
}
