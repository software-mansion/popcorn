import type { LLVView } from "../types";

/**
 * Registry of the fake Phoenix views LLV mounts, keyed by element id. Replaces
 * the bare `Record<string, LLVView>` that used to be threaded through every
 * setup function and mutated in place.
 */
export class ViewRegistry {
  private byId = new Map<string, LLVView>();

  get(id: string): LLVView | undefined {
    return this.byId.get(id);
  }

  has(id: string): boolean {
    return this.byId.has(id);
  }

  add(id: string, view: LLVView): void {
    this.byId.set(id, view);
  }

  /** Remove and return the view, or undefined if it wasn't registered. */
  remove(id: string): LLVView | undefined {
    const view = this.byId.get(id);
    this.byId.delete(id);
    return view;
  }

  ids(): string[] {
    return [...this.byId.keys()];
  }
}
