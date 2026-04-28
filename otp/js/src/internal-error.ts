export class PopcornInternalError extends Error {
  constructor(
    public readonly code: string,
    message?: string,
  ) {
    super(message ?? `Internal error: ${code}`);
    this.name = "PopcornInternalError";
  }
}
