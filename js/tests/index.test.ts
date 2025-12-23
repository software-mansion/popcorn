import { expect, test } from "vitest";
import { fn } from "..";

test("fn", () => {
  expect(fn()).toBe("Hello, tsdown!");
});
