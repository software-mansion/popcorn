// Opens Livebook in headless Chromium, creates notebooks and evaluates cells.
// Hosts public/ with serve.exs, unless given another server's URL.
// Uses Playwright from popcorn/js. Usage: node scripts/check.mjs [url]
import { spawn } from "node:child_process";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

const require = createRequire(new URL("../../../popcorn/js/package.json", import.meta.url));
const { chromium } = require("@playwright/test");

const server = process.argv[2] ? null : await serve();
const url = process.argv[2] ?? server.url;

// On a free port, which serve.exs prints
function serve() {
  const child = spawn("elixir", ["serve.exs", "0"], {
    cwd: fileURLToPath(new URL("..", import.meta.url)),
    stdio: ["ignore", "pipe", "inherit"],
  });
  return new Promise((resolve, reject) => {
    let printed = "";
    child.stdout.on("data", (data) => {
      printed += data;
      const port = printed.match(/http:\/\/localhost:(\d+)/)?.[1];
      if (port) resolve({ child, url: `http://localhost:${port}/` });
    });
    child.on("exit", (code) => reject(new Error(`serve.exs exited with status ${code}`)));
  });
}
const browser = await chromium.launch();
const context = await browser.newContext({ viewport: { width: 1280, height: 900 } });
const page = await context.newPage();
const logs = [];
page.on("console", (msg) => {
  logs.push(`[${msg.type()}] ${msg.text()}`);
  if (process.env.VERBOSE) console.log(`  [console] ${msg.text().slice(0, 500)}`);
});
page.on("pageerror", (err) => logs.push(`[pageerror] ${err.message}`));

let ok = true;
const step = async (name, fn) => {
  const t0 = Date.now();
  try {
    const detail = await fn();
    console.log(`PASS ${name} (${Date.now() - t0} ms)${detail ? `: ${detail}` : ""}`);
  } catch (error) {
    ok = false;
    const message = String(error.message);
    // A cell's error is worth reading whole, Playwright's call logs aren't
    const shown = message.startsWith("the cell raised") ? message : message.split("\n")[0];
    console.log(`FAIL ${name}: ${shown.slice(0, 4000)}`);
    throw error;
  }
};

const app = page.frameLocator("#livebook");
// Livebook's paths are under the tab's
const tabPath = (frame) => new URL(frame.url()).pathname.match(/^\/tab\/[^/]+/)[0];
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

  const output = cell.locator("[data-el-outputs-container]");

  const run = async (code) => {
    await cell.locator(".cm-content").click();
    await page.keyboard.press("ControlOrMeta+A");
    await page.keyboard.type(code);
    // Like a person: Livebook evaluates the source it has synced from the editor
    await page.waitForTimeout(500);
    await page.keyboard.press("ControlOrMeta+Enter");
  };

  const evaluate = async (code, expected, timeout = 120_000) => {
    await run(code);
    const t0 = Date.now();
    const printed = new Set();

    // Prints the cell's output while it runs, like Mix.install's
    const progress = setInterval(async () => {
      const text = await output.innerText().catch(() => "");
      for (const line of text.split("\n").filter((line) => line.trim())) {
        if (printed.has(line)) continue;
        printed.add(line);
        console.log(`  [${Math.round((Date.now() - t0) / 1000)} s] ${line}`);
      }
    }, 5_000);

    const evaluated = output.getByText(expected).filter({ visible: true }).first().waitFor({ timeout });

    // Fails as soon as the cell raises, rather than waiting for the timeout
    const raised = output
      .getByText("** (")
      .first()
      .waitFor({ timeout })
      .then(
        async () => {
          throw new Error(`the cell raised:\n${await output.innerText()}`);
        },
        () => new Promise(() => {}),
      );

    try {
      await Promise.race([evaluated, raised]);
    } finally {
      clearInterval(progress);
    }
    return (await output.innerText()).trim();
  };

  // The first evaluation starts the notebook's runtime, in a VM of its own
  await step("a cell evaluates in the notebook's VM", () => evaluate("Enum.sum(1..10)", "55"));

  let runtimeNode;
  await step("the code runs in a browser VM other than Livebook's", async () => {
    const output = await evaluate(
      "{node(), :erlang.system_info(:system_architecture)}",
      "wasm32-unknown-emscripten",
    );
    runtimeNode = output.match(/runtime_\d+@popcorn/)?.[0];
    if (!runtimeNode) throw new Error(`not the runtime's node: ${output}`);
    const booted = logs.find((line) => line.includes(`Runtime VM ${runtimeNode} booted`));
    return `${output}, ${booted?.match(/booted in \d+ ms/)?.[0]}`;
  });

  // Kino, a Mix package and a rebar3-only one, which Livebook doesn't depend on,
  // and Req, with Mint, whose Erlang code includes OTP's headers
  await step("Mix.install downloads and compiles packages", () =>
    evaluate(
      `Mix.install([{:kino, "~> 0.18.0"}, {:nimble_csv, "~> 1.2"}, {:jsone, "~> 1.9"}, :req])`,
      ":ok",
      600_000,
    ),
  );

  await step("Kino.JS outputs render in their iframes", async () => {
    await run(`Kino.HTML.new("<p>Rendered by Kino.JS</p>")`);
    // Livebook keeps outputs' iframes outside the cells, over their placeholders
    const frame = app.locator("iframe[src*='/iframe/v6.html']").first().contentFrame();
    await frame.getByText("Rendered by Kino.JS").waitFor({ timeout: 120_000 });
  });

  // Each tab runs a Livebook of its own, under a path with a Service Worker of
  // its own. Only the first tab's VM has its notebook.
  await step("each tab's requests and navigations go to its own VM", async () => {
    const other = await context.newPage();
    other.on("console", (msg) => logs.push(`[other tab] [${msg.type()}] ${msg.text()}`));
    try {
      await other.goto(url);
      await other.waitForFunction(() => window.livebookStarted, null, { timeout: 180_000 });
      await other.frameLocator("#livebook").locator(".phx-connected").first().waitFor({ timeout: 60_000 });

      // Whether a notebook is under the frame's tab's path
      const found = (frame, path) =>
        frame.evaluate(async (path) => {
          const base = window.LIVEBOOK_BASE_URL_PATH ?? window.parent.LIVEBOOK_BASE_URL_PATH;
          const resp = await fetch(base + path);
          return resp.ok && !resp.redirected;
        }, path);

      const notebook = page.frames().find((frame) => frame.url().includes("/sessions/"));
      const output = page.frames().find((frame) => frame.url().includes("/iframe/v6.html"));
      const otherTab = await (await other.$("#livebook")).contentFrame();
      const path = new URL(notebook.url()).pathname.slice(tabPath(notebook).length);

      const results = {
        notebook: await found(notebook, path),
        output: await found(output, path),
        otherTab: await found(otherTab, path),
      };
      if (!results.notebook || !results.output || results.otherTab) {
        throw new Error(`whether ${path} was found: ${JSON.stringify(results)}`);
      }

      // The other tab was opened last, which mustn't matter
      const reloaded = page.waitForEvent("framenavigated", (frame) => frame === notebook);
      await notebook.evaluate(() => location.reload());
      await reloaded;
      await app.locator(".phx-connected [data-el-cell]").first().waitFor({ timeout: 60_000 });
      if (!notebook.url().endsWith(path)) throw new Error(`reloading went to ${notebook.url()}`);
      return `${path} reloaded`;
    } finally {
      await other.close();
    }
  });

  await step("installed packages run", () =>
    evaluate(
      `{NimbleCSV.RFC4180.parse_string("a,b\\n1,2\\n"), :jsone.decode("[1,2]")}`,
      `{[["1", "2"]], [1, 2]}`,
    ),
  );

  // With Popcorn's adapter, which it installs once Mix.install/2 has started Req.
  // jsDelivr compresses the JSON, which the browser decompresses.
  await step("Req requests with the browser's fetch()", () =>
    evaluate(
      `resp = Req.get!("https://cdn.jsdelivr.net/npm/vega-datasets@2/data/gapminder.json")\n` +
        `{Req.default_options()[:adapter], resp.status, is_list(resp.body)}`,
      "{Popcorn.Fetch, 200, true}",
    ),
  );

  // Mix.install/2 installs once per VM
  await step("another notebook installs other packages in a VM of its own", async () => {
    const session = page.frames().find((frame) => frame.url().includes("/sessions/"));
    await session.goto(new URL(`${tabPath(session)}/new`, url).href);
    await cell.locator(".cm-content").waitFor({ timeout: 60_000 });

    const output = await evaluate(
      `Mix.install([{:jason, "~> 1.4"}])\n{node(), Jason.encode!(%{installed: true})}`,
      "@popcorn",
      600_000,
    );
    if (output.includes(runtimeNode)) throw new Error(`the first notebook's VM: ${output}`);
    if (!output.includes(`{\\"installed\\":true}`)) throw new Error(`no Jason: ${output}`);
    runtimeNode = output.match(/runtime_\d+@popcorn/)[0];
    return output.split("\n").at(-1);
  });

  await step("reconnecting replaces the notebook's VM", async () => {
    await app.locator("[data-el-runtime-info-toggle]").click();
    await app.locator("[data-el-runtime-info]").getByRole("button", { name: "Reconnect" }).click();

    // A result that the cell's previous output doesn't have
    const output = await evaluate("{:reconnected, node()}", ":reconnected");
    if (output.includes(runtimeNode)) throw new Error(`the same VM: ${output}`);

    const stopped = `Runtime VM ${runtimeNode} stopped`;
    for (let i = 0; !logs.some((line) => line.includes(stopped)); i++) {
      if (i > 100) throw new Error(`${runtimeNode} kept running`);
      await page.waitForTimeout(100);
    }
    return `${runtimeNode} stopped, now ${output}`;
  });

  // Where livebook.dev's "Run in Livebook" badges send people, with the
  // example's address as their Livebook's
  await step("a link opens a notebook from GitHub", async () => {
    const notebook =
      "https://github.com/livebook-dev/livebook/blob/main/lib/livebook/notebook/learn/intro_to_livebook.livemd";
    const linked = await context.newPage();
    linked.on("console", (msg) => logs.push(`[linked tab] [${msg.type()}] ${msg.text()}`));
    try {
      await linked.goto(new URL(`/import?url=${encodeURIComponent(notebook)}`, url).href);
      await linked.waitForFunction(() => window.livebookStarted, null, { timeout: 180_000 });
      await linked.waitForFunction(
        () => document.querySelector("#livebook")?.contentWindow.location.pathname.includes("/sessions/"),
        null,
        { timeout: 60_000 },
      );
      const app = linked.frameLocator("#livebook");
      await app.getByText("Welcome to Livebook").first().waitFor({ timeout: 60_000 });

      // So that reloading doesn't open the link again
      await linked.waitForFunction(() => location.pathname + location.search === "/", null, { timeout: 10_000 });
      return `${await app.locator("[data-el-cell]").count()} cells, the page at ${linked.url()}`;
    } finally {
      await linked.close();
    }
  });
} catch {
  console.log("--- browser console ---");
  for (const line of logs.slice(-120)) console.log(line.length > 800 ? line.slice(0, 800) + "…" : line);
  await page.screenshot({ path: new URL("../check-failure.png", import.meta.url).pathname });
}

await browser.close();
server?.child.kill();
process.exit(ok ? 0 : 1);
