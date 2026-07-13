import type { CookieScriptApi } from "./consent";

declare global {
  interface Window {
    CookieScript?: CookieScriptApi;
    /** Revive delivery loaders, keyed by the zones' shared content id. */
    contentAsync?: Record<string, { dispatchEvent(event: string): void }>;
  }
}

export {};
