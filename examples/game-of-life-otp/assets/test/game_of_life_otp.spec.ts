import { expect, test, type Page } from "@playwright/test";

const READY_TIMEOUT = 30_000;

test("renders the grid and runs a glider simulation", async ({ page }) => {
  await page.goto("/");

  await expect(page.locator(".cell")).toHaveCount(400, {
    timeout: READY_TIMEOUT,
  });

  await page.locator("#glider").click();
  await expect(page.locator(".cell-alive")).toHaveCount(5);

  await page.locator("#start").click();
  await expect(page.locator("#stop")).toBeVisible();

  // The glider stays 5 cells while moving; wait for it to leave its
  // starting position.
  const initialCell = page.locator('[data-coords-x="2"][data-coords-y="0"]');
  await expect(initialCell).not.toHaveClass(/cell-alive/, { timeout: 10_000 });
  await expect(page.locator(".cell-alive")).toHaveCount(5);

  await page.locator("#stop").click();
  await expect(page.locator("#start")).toBeVisible();

  const stoppedCells = await liveCellCoordinates(page);
  await page.waitForTimeout(700);
  expect(await liveCellCoordinates(page)).toEqual(stoppedCells);
});

test("toggles cells by clicking when stopped", async ({ page }) => {
  await page.goto("/");

  const cell = page.locator('[data-coords-x="5"][data-coords-y="5"]');
  await expect(cell).toBeVisible({ timeout: READY_TIMEOUT });

  await cell.click();
  await expect(cell).toHaveClass(/cell-alive/);

  await cell.click();
  await expect(cell).not.toHaveClass(/cell-alive/);
});

async function liveCellCoordinates(page: Page) {
  return page.locator(".cell-alive").evaluateAll((cells) =>
    cells.map((cell) => [
      cell.getAttribute("data-coords-x"),
      cell.getAttribute("data-coords-y"),
    ]),
  );
}
