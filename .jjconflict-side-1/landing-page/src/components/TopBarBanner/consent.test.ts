import { describe, expect, it } from "vitest";
import { hasTargetingConsent, type CookieScriptApi } from "./consent";

const cookieScript = (categories: string[]): CookieScriptApi => ({
  instance: {
    currentState: () => ({ categories }),
  },
});

describe("targeting consent", () => {
  it("requires the targeting category", () => {
    expect(hasTargetingConsent()).toBe(false);
    expect(hasTargetingConsent(cookieScript(["strict", "performance"]))).toBe(
      false,
    );
    expect(hasTargetingConsent(cookieScript(["strict", "targeting"]))).toBe(
      true,
    );
  });
});
