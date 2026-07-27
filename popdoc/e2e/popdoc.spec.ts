import { test, expect, devices, Page, Locator } from "@playwright/test";

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

// Prompts are decorated before the WASM runtime finishes booting, but click
// handlers exist only once popdoc marks them bound — interacting earlier is
// silently lost.
async function awaitPromptReady(prompt: Locator) {
  await expect(prompt).toHaveAttribute("data-popdoc-iex-bound", "true", {
    timeout: RUN_READY_TIMEOUT_MS,
  });
}

async function runPrompt(prompt: Locator) {
  await awaitPromptReady(prompt);
  await prompt.click();
  await expect(prompt).toHaveAttribute("data-iex-state", "success", {
    timeout: EVAL_TIMEOUT_MS,
  });
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

test("decorates iex-popcorn prompts as clickable and opens the IEx terminal", async ({
  page,
}) => {
  const prompt = page.locator(".popdoc-iex-prompt").first();
  await expect(prompt).toBeAttached({ timeout: RUN_READY_TIMEOUT_MS });
  await expect(prompt).toHaveAttribute("title", "Run in IEx");

  const terminal = page.locator(".popdoc-terminal");
  await expect(terminal).toBeAttached({ timeout: RUN_READY_TIMEOUT_MS });
  await expect(terminal).not.toHaveClass(/popdoc-terminal--open/);

  await runPrompt(prompt);
  await expect(terminal).toHaveClass(/popdoc-terminal--open/);
});

test("resets the IEx session, wiping the screen and prompt state", async ({
  page,
}) => {
  const prompt = page.locator(".popdoc-iex-prompt").first();
  await runPrompt(prompt);

  const reset = page.locator(".popdoc-terminal-btn", { hasText: "Reset" });
  await reset.click();
  await expect(prompt).not.toHaveAttribute("data-iex-state");

  // The restarted runtime prints a fresh prompt; the old output is gone.
  const rows = page.locator(".popdoc-terminal .xterm-rows");
  await expect(rows).toContainText("iex(", { timeout: EVAL_TIMEOUT_MS });
  await expect(rows).not.toContainText("x = 1 + 1");
});

test("clears only the terminal screen, keeping session and prompt state", async ({
  page,
}) => {
  const prompt = page.locator(".popdoc-iex-prompt").first();
  await runPrompt(prompt);

  const clear = page.locator(".popdoc-terminal-btn", { hasText: "Clear" });
  await clear.click();

  const rows = page.locator(".popdoc-terminal .xterm-rows");
  await expect(rows).not.toContainText("x = 1 + 1");
  // clear() keeps the cursor's line, so the live prompt survives.
  await expect(rows).toContainText("iex(");
  await expect(prompt).toHaveAttribute("data-iex-state", "success");
  await expect(page.locator(".popdoc-terminal")).toHaveClass(
    /popdoc-terminal--open/,
  );
});

test("shows the iex launcher and opens the terminal from it", async ({
  page,
}) => {
  const launcher = page.locator(".popdoc-iex-launcher");
  await expect(launcher).toBeVisible({ timeout: RUN_READY_TIMEOUT_MS });

  const terminal = page.locator(".popdoc-terminal");
  await launcher.click();
  await expect(terminal).toHaveClass(/popdoc-terminal--open/);
  await expect(launcher).toBeHidden();

  await page.locator(".popdoc-terminal-btn", { hasText: "✕" }).click();
  await expect(terminal).not.toHaveClass(/popdoc-terminal--open/);
  await expect(launcher).toBeVisible();
});

test("launcher starts an IEx session on pages without popcorn blocks", async ({
  page,
}) => {
  await page.goto("/Example.html");
  await page.waitForLoadState("networkidle");

  const launcher = page.locator(".popdoc-iex-launcher");
  await expect(launcher).toBeVisible({ timeout: RUN_READY_TIMEOUT_MS });
  await launcher.click();

  await expect(page.locator(".popdoc-terminal")).toHaveClass(
    /popdoc-terminal--open/,
  );
  await expect(page.locator(".popdoc-terminal .xterm-rows")).toContainText(
    "iex(",
    { timeout: EVAL_TIMEOUT_MS },
  );
});

test("collapses and closes the IEx terminal", async ({ page }) => {
  const prompt = page.locator(".popdoc-iex-prompt").first();
  await awaitPromptReady(prompt);
  await prompt.click();

  const terminal = page.locator(".popdoc-terminal");
  await expect(terminal).toHaveClass(/popdoc-terminal--open/);

  const collapse = page.locator(".popdoc-terminal-collapse");
  await collapse.click();
  await expect(terminal).toHaveClass(/popdoc-terminal--collapsed/);

  await collapse.click();
  await expect(terminal).not.toHaveClass(/popdoc-terminal--collapsed/);

  await page.locator(".popdoc-terminal-btn", { hasText: "✕" }).click();
  await expect(terminal).not.toHaveClass(/popdoc-terminal--open/);
});

test("reveals the next iex> prompt only after the previous one succeeds", async ({
  page,
}) => {
  const block = page.locator("pre.popcorn-iex").first();
  const prompts = block.locator(".popdoc-iex-prompt");
  await expect(prompts).toHaveCount(2, { timeout: RUN_READY_TIMEOUT_MS });

  const first = prompts.nth(0);
  const second = prompts.nth(1);

  await expect(first).toHaveClass(/popdoc-iex-prompt--runnable/);
  await expect(second).not.toHaveClass(/popdoc-iex-prompt--runnable/);

  await runPrompt(first);
  await expect(first).not.toHaveClass(/popdoc-iex-prompt--runnable/);
  await expect(second).toHaveClass(/popdoc-iex-prompt--runnable/);
});

test("scopes iex> execution to each code block independently", async ({
  page,
}) => {
  const blocks = page.locator("pre.popcorn-iex");
  await expect(blocks).toHaveCount(2, { timeout: RUN_READY_TIMEOUT_MS });

  const secondBlockPrompt = blocks.nth(1).locator(".popdoc-iex-prompt").first();
  await expect(secondBlockPrompt).toHaveClass(/popdoc-iex-prompt--runnable/);

  await runPrompt(secondBlockPrompt);

  const firstBlockPrompts = blocks.nth(0).locator(".popdoc-iex-prompt");
  await expect(firstBlockPrompts.nth(0)).not.toHaveAttribute("data-iex-state");
  await expect(firstBlockPrompts.nth(1)).not.toHaveAttribute("data-iex-state");
});

test("clicking a later iex> in the same block runs earlier prompts first", async ({
  page,
}) => {
  const block = page.locator("pre.popcorn-iex").first();
  const prompts = block.locator(".popdoc-iex-prompt");
  await expect(prompts).toHaveCount(2, { timeout: RUN_READY_TIMEOUT_MS });

  await runPrompt(prompts.nth(1));
  // The chain runs earlier prompts first, so by now the first one succeeded.
  await expect(prompts.nth(0)).toHaveAttribute("data-iex-state", "success");
});

test("marks continuation lines and runs the whole multi-line command", async ({
  page,
}) => {
  const block = page.locator("pre.popcorn-iex").nth(1);
  const conts = block.locator(".popdoc-iex-prompt--cont");
  await expect(conts).toHaveCount(2, { timeout: RUN_READY_TIMEOUT_MS });

  // Bound state is marked on the main prompt; conts are wired in the same
  // pass.
  await awaitPromptReady(
    block.locator(".popdoc-iex-prompt:not(.popdoc-iex-prompt--cont)").first(),
  );
  await conts.first().click();
  await expect(conts.first()).toHaveAttribute("data-iex-state", "success", {
    timeout: EVAL_TIMEOUT_MS,
  });

  const pills = block.locator(
    ".popdoc-iex-prompt:not(.popdoc-iex-prompt--cont)",
  );
  await expect(pills.nth(0)).toHaveAttribute("data-iex-state", "success");
  await expect(pills.nth(1)).toHaveAttribute("data-iex-state", "success");
  await expect(pills.nth(2)).toHaveClass(/popdoc-iex-prompt--runnable/);
});

test("shows the run affordance without hover", async ({ page }) => {
  const runnable = page.locator(".popdoc-iex-prompt--runnable").first();
  await expect(runnable).toBeAttached({ timeout: RUN_READY_TIMEOUT_MS });

  const icon = runnable.locator(".popdoc-iex-icon");
  const content = await icon.evaluate(
    (el) => getComputedStyle(el, "::before").content,
  );
  expect(content).toBe('"▶"');
});

test("keeps decorations out of the copied text", async ({ page }) => {
  const block = page.locator("pre.popcorn-iex").first();
  await expect(block.locator(".popdoc-iex-prompt").first()).toBeAttached({
    timeout: RUN_READY_TIMEOUT_MS,
  });

  const code = block.locator("code");
  const text = await code.evaluate((el) =>
    Array.from(el.children)
      .map((c) => c.textContent)
      .join(""),
  );
  expect(text).toContain("iex> ");
  expect(text).not.toMatch(/[▶✓]/);
});

test.describe("touch", () => {
  // defaultBrowserType cannot change inside a describe (forces a new worker);
  // keep the Pixel 7 viewport/touch traits and let the project pick the browser.
  const { defaultBrowserType: _browser, ...pixel7 } = devices["Pixel 7"];
  test.use(pixel7);

  test("prompt runs on tap", async ({ page }) => {
    const prompt = page.locator(".popdoc-iex-prompt").first();
    await awaitPromptReady(prompt);
    await prompt.tap();
    await expect(prompt).toHaveAttribute("data-iex-state", "success", {
      timeout: EVAL_TIMEOUT_MS,
    });
  });
});

test("previews the chain of commands that will run on hover", async ({
  page,
}) => {
  const block = page.locator("pre.popcorn-iex").first();
  const prompts = block.locator(
    ".popdoc-iex-prompt:not(.popdoc-iex-prompt--cont)",
  );
  await expect(prompts).toHaveCount(2, { timeout: RUN_READY_TIMEOUT_MS });

  // Hover mirroring is bound together with the click handlers.
  await awaitPromptReady(prompts.nth(1));
  await prompts.nth(1).hover();
  await expect(prompts.nth(0)).toHaveClass(/popdoc-iex-hover-chain/);

  await page.mouse.move(0, 0);
  await expect(prompts.nth(0)).not.toHaveClass(/popdoc-iex-hover-chain/);
});
