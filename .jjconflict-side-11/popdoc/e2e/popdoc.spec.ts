import { test, expect, Page, Locator } from "@playwright/test";

const RUN_READY_TIMEOUT_MS = 60_000;
const EVAL_TIMEOUT_MS = 60_000;

test.beforeEach(async ({ page }) => {
  await page.goto("/readme.html");
  await page.waitForLoadState("networkidle");
});

async function getBlock(page: Page, index: number): Promise<Locator> {
  const block = page.locator(".popdoc-block").nth(index);
  await expect(block).toBeAttached();
  return block;
}

async function runBlock(block: Locator) {
  const run = block.locator(".popdoc-run");
  await expect(run).toBeEnabled({ timeout: RUN_READY_TIMEOUT_MS });
  await run.click();
  await expect(run).toBeEnabled({ timeout: EVAL_TIMEOUT_MS });
}

test("decorates elixir-popcorn blocks and enables Run once Popcorn is ready", async ({
  page,
}) => {
  const blocks = page.locator(".popdoc-block");
  await expect(blocks).toHaveCount(4);

  const firstRun = blocks.first().locator(".popdoc-run");
  await expect(firstRun).toBeEnabled({ timeout: RUN_READY_TIMEOUT_MS });
});

test("evaluates a single expression and renders the result", async ({
  page,
}) => {
  const block = await getBlock(page, 0);
  await runBlock(block);

  const topResult = block.locator(".popdoc-top .popdoc-result");
  await expect(topResult).toContainText("42");

  const status = block.locator(".popdoc-status");
  await expect(status).toContainText("ok");
  await expect(status.locator(".popdoc-dot-ok")).toBeAttached();
});

test("evaluates multi-expression block with stdout, stderr, and bindings", async ({
  page,
}) => {
  const block = await getBlock(page, 1);
  await runBlock(block);

  const topResult = block.locator(".popdoc-top .popdoc-result");
  await expect(topResult).toContainText(":non_empty");

  const stdout = block.locator(".popdoc-stdio").filter({ hasText: "STDOUT" });
  await expect(stdout).toContainText("hello");
  await expect(stdout).toContainText("from");
  await expect(stdout).toContainText("stdout");

  const stderr = block.locator(".popdoc-stdio.popdoc-stderr");
  await expect(stderr).toContainText("hello from stderr");

  const rows = block.locator(".popdoc-list .popdoc-row");
  await expect(rows.first().locator(".popdoc-cell-result")).toContainText("x = 42");
});

test("renders runtime error with status and bindings on prior rows", async ({
  page,
}) => {
  const block = await getBlock(page, 2);
  await runBlock(block);

  const errorTop = block.locator(".popdoc-top-error");
  await expect(errorTop).toBeAttached();
  await expect(errorTop.locator(".popdoc-result-err")).toContainText(
    /ArithmeticError|bad argument|division/i,
  );

  const status = block.locator(".popdoc-status");
  await expect(status).toContainText("error");
  await expect(status.locator(".popdoc-dot-err")).toBeAttached();

  const rows = block.locator(".popdoc-list .popdoc-row");
  await expect(rows.nth(2).locator(".popdoc-cell-result")).toContainText("c = 10");
});

test("renders error with a stacktrace toggle for nested failures", async ({
  page,
}) => {
  const block = await getBlock(page, 3);
  await runBlock(block);

  const errorTop = block.locator(".popdoc-top-error");
  await expect(errorTop).toBeAttached();

  const toggle = block.locator(".popdoc-stacktrace-toggle");
  await expect(toggle).toBeAttached();

  const trace = block.locator(".popdoc-stacktrace");
  await expect(trace).toBeHidden();
  await toggle.click();
  await expect(trace).toBeVisible();
  await expect(trace).toContainText(/Example|safe_div|check_denominator/);
});
