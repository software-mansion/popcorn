export interface CookieScriptState {
  categories?: string[];
}

export interface CookieScriptApi {
  instance?: {
    currentState?: () => CookieScriptState;
  };
}

export const hasTargetingConsent = (cookieScript?: CookieScriptApi) =>
  cookieScript?.instance?.currentState?.().categories?.includes("targeting") ===
  true;
