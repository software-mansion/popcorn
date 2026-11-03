// Opens Livebook in headless Chromium, creates a notebook and evaluates a cell.
// Uses Playwright from popcorn/js. Usage: node scripts/check.mjs [url]
import { createRequire } from "node:module";

const require = createRequire(new URL("../../../popcorn/js/package.json", import.meta.url));
const { chromium } = require("@playwright/test");

const url = process.argv[2] ?? "http://localhost:4000/";
const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
const logs = [];
page.on("console", (msg) => logs.push(`[${msg.type()}] ${msg.text()}`));
page.on("pageerror", (err) => logs.push(`[pageerror] ${err.message}`));

let ok = true;
const step = async (name, fn) => {
  const t0 = Date.now();
  try {
    const detail = await fn();
    console.log(`PASS ${name} (${Date.now() - t0} ms)${detail ? `: ${detail}` : ""}`);
  } catch (error) {
    ok = false;
    console.log(`FAIL ${name}: ${String(error.message).split("\n")[0]}`);
    throw error;
  }
};

const app = page.frameLocator("iframe");
const newNotebook = () => app.getByText("New notebook").filter({ visible: true }).first();

try {
  await page.goto(url);

  await step("Livebook starts in the VM", async () => {
    await page.waitForFunction(() => window.livebookStarted, null, { timeout: 180_000 });
    const started = await page.evaluate(() => window.livebookStarted);
    return `${started.ms} ms, node ${started.node}`;
  });

  await step("home page renders and connects", async () => {
    await newNotebook().waitFor({ timeout: 60_000 });
    await app.locator(".phx-connected").first().waitFor({ timeout: 60_000 });
  });

  await step("new notebook opens", async () => {
    await newNotebook().click();
    await app.locator("[data-el-cell][data-type='code'] .cm-content").first().waitFor({ timeout: 60_000 });
    return page.frames().find((frame) => frame.url().includes("/sessions/"))?.url();
  });

  // The new notebook's first code cell is the setup cell
  const cell = app.locator("[data-el-cell][data-type='code']").last();

  const evaluate = async (code, expected) => {
    await cell.locator(".cm-content").click();
    await page.keyboard.press("ControlOrMeta+A");
    await page.keyboard.type(code);
    // Like a person: Livebook evaluates the source it has synced from the editor
    await page.waitForTimeout(500);
    await page.keyboard.press("ControlOrMeta+Enter");
    const output = cell.locator("[data-el-outputs-container]");
    await output.getByText(expected).filter({ visible: true }).first().waitFor({ timeout: 120_000 });
    return (await output.innerText()).trim();
  };

  await step("a cell evaluates in the embedded runtime", () => evaluate("Enum.sum(1..10)", "55"));

  await step("the code runs in the browser's VM", () =>
    evaluate(":erlang.system_info(:system_architecture)", "wasm32"),
  );
} catch {
  console.log("--- browser console ---");
  for (const line of logs.slice(-120)) console.log(line.length > 800 ? line.slice(0, 800) + "…" : line);
  await page.screenshot({ path: new URL("../check-failure.png", import.meta.url).pathname });
}

await browser.close();
process.exit(ok ? 0 : 1);
