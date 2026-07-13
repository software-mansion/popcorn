import { expect, test, type Page } from "@playwright/test";

const READY_TIMEOUT = 30_000;

type TerminalBuffer = {
  length: number;
  getLine(y: number): { translateToString(trimRight?: boolean): string } | undefined;
};

declare global {
  interface Window {
    iexWasm: { terminal: { buffer: { active: TerminalBuffer } } };
  }
}

function screen(page: Page): Promise<string> {
  return page.evaluate(() => {
    const buffer = window.iexWasm.terminal.buffer.active;
    const lines = [];

    for (let y = 0; y < buffer.length; y++) {
      lines.push(buffer.getLine(y)?.translateToString(true) ?? "");
    }

    return lines.join("\n");
  });
}

test("evaluates in the IEx shell", async ({ page }) => {
  await page.goto("/");

  await expect(page.locator("html")).toHaveAttribute("data-popcorn-ready", "true", {
    timeout: READY_TIMEOUT,
  });
  await expect.poll(() => screen(page), { timeout: READY_TIMEOUT }).toContain("iex(1)>");

  await page.locator("#elixir-terminal").click();
  await page.keyboard.type("1 + 1");
  await page.keyboard.press("Enter");

  await expect.poll(() => screen(page), { timeout: READY_TIMEOUT }).toMatch(/^2$/m);
});
