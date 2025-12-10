export function trace(name: string, args?: Record<string, string>): void {
  if (args === undefined) {
    console.debug(name);
  } else {
    console.debug(name, args);
  }
}
