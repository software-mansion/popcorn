// Runs the page's checks in headless Chromium, with Playwright from popcorn/js.
// Usage: node scripts/check.mjs [url]
import { createRequire } from "node:module";

const require = createRequire(new URL("../../../popcorn/js/package.json", import.meta.url));
const { chromium } = require("@playwright/test");

const url = process.argv[2] ?? "http://localhost:4000/";
const browser = await chromium.launch();
const page = await browser.newPage();
const logs = [];
page.on("console", (msg) => logs.push(`[${msg.type()}] ${msg.text()}`));
page.on("pageerror", (err) => logs.push(`[pageerror] ${err.message}`));

await page.goto(url);
try {
  await page.waitForFunction(() => window.spikeDone, null, { timeout: 120_000 });
} catch {
  logs.push("[check] timed out waiting for the page to finish");
}

const results = await page.evaluate(() => window.spikeResults ?? []);
for (const { name, ok, detail } of results) {
  console.log(`${ok ? "PASS" : "FAIL"} ${name}${detail ? `: ${detail}` : ""}`);
}

const passed = results.length > 0 && results.every((result) => result.ok);
if (!passed) {
  console.log("--- browser console ---");
  for (const line of logs.slice(0, 200)) console.log(line);
}

await browser.close();
process.exit(passed ? 0 : 1);
