export function log(message: string): void {
  const context = window.parent !== window ? "[Iframe]" : "[Main]";
  console.log(`${context} ${message}`);
}

export function createElement(
  tag: string,
  attributes?: Record<string, string>,
): HTMLElement {
  const element = document.createElement(tag);
  if (attributes) {
    Object.entries(attributes).forEach(([key, value]) => {
      element.setAttribute(key, value);
    });
  }
  return element;
}

export function generateId(): string {
  return Math.random().toString(36).substring(2, 15);
}
