// Opens Livebook in its browser VM, creates notebooks and evaluates cells in
// them. The tests build on each other: they share one page, its Livebook and
// the notebooks' VMs, so a failure skips the rest.
import { expect, test } from "@playwright/test";

test.describe.configure({ mode: "serial" });

let context;
let page;
let app;
// The current notebook's last code cell, and functions that evaluate code in
// it, see notebook/1
let cell;
let output;
let run;
let evaluate;
// The browser console's messages, from all of the tests' pages
const logs = [];
// The node of the current notebook's VM
let runtimeNode;

const watch = (page, prefix = "") => {
  page.on("console", (msg) => {
    logs.push(`${prefix}[${msg.type()}] ${msg.text()}`);
    if (process.env.VERBOSE)
      console.log(`  [console] ${prefix}${msg.text().slice(0, 500)}`);
  });
  page.on("pageerror", (err) =>
    logs.push(`${prefix}[pageerror] ${err.message}`),
  );
};

test.beforeAll(async ({ browser }, testInfo) => {
  const { baseURL, viewport } = testInfo.project.use;
  context = await browser.newContext({ baseURL, viewport });
  page = await context.newPage();
  watch(page);
  app = page.frameLocator("#livebook");
  ({ cell, output, run, evaluate } = notebook(page));
});

test.afterEach(async ({}, testInfo) => {
  if (testInfo.status === testInfo.expectedStatus) return;
  const lines = logs
    .slice(-120)
    .map((line) => (line.length > 800 ? line.slice(0, 800) + "…" : line));
  await testInfo.attach("browser console", {
    body: lines.join("\n"),
    contentType: "text/plain",
  });
  await testInfo.attach("screenshot", {
    body: await page.screenshot(),
    contentType: "image/png",
  });
});

test.afterAll(async () => {
  await context?.close();
});

// Livebook's paths are under the tab's
const tabPath = (frame) =>
  new URL(frame.url()).pathname.match(/^\/tab\/[^/]+/)[0];
const newNotebook = () =>
  app.getByText("New notebook").filter({ visible: true }).first();
const sessionFrame = () =>
  page.frames().find((frame) => frame.url().includes("/sessions/"));

// The last code cell of the notebook open in a page's Livebook, and functions
// that evaluate code in it. A new notebook's first code cell is the setup cell.
const notebook = (page) => {
  const cell = page
    .frameLocator("#livebook")
    .locator("[data-el-cell][data-type='code']")
    .last();
  const output = cell.locator("[data-el-outputs-container]");

  const run = async (code) => {
    await cell.locator(".cm-content").click();
    await page.keyboard.press("ControlOrMeta+A");
    await page.keyboard.type(code);
    // Like a person: Livebook evaluates the source it has synced from the editor
    await page.waitForTimeout(500);
    await page.keyboard.press("ControlOrMeta+Enter");
  };

  // Evaluates the code in the cell, and returns its output once it has the
  // expected text
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

    const evaluated = output
      .getByText(expected)
      .filter({ visible: true })
      .first()
      .waitFor({ timeout });

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

  return { cell, output, run, evaluate };
};

test("Livebook starts in the VM", async () => {
  await page.goto("/");
  // Popcorn's button shows that Livebook is loading, with its card closed
  await expect(page.locator("#popcorn")).toHaveAttribute(
    "data-state",
    "loading",
  );
  await expect(page.locator("#card")).toBeHidden();
  await page.waitForFunction(() => window.livebookStarted, null, {
    timeout: 180_000,
  });
  const started = await page.evaluate(() => window.livebookStarted);
  console.log(`  Livebook started in ${started.ms} ms, as ${started.node}`);

  // The card shows Livebook's state, and later its notebooks' VMs
  await expect(page.locator("#status")).toHaveAttribute("data-state", "ready");
  await expect(page.locator("#status-text")).toHaveText("Running");
  await expect(page.locator("#notebook-vms")).toHaveText("None");
  await expect(page.locator("#card")).toBeHidden();
  // The loader that covered the page goes
  await expect(page.locator("#loader:not(.done)")).toHaveCount(0);
});

test("the home page renders and connects", async () => {
  await expect(newNotebook()).toBeVisible();
  await expect(app.locator(".phx-connected").first()).toBeVisible();
});

// Popcorn's button toggles its card, which Escape and clicking Livebook's page
// close too
test("Popcorn's button opens its card", async () => {
  const button = page.locator("#popcorn");
  const card = page.locator("#card");
  for (let i = 0; i < 3; i++) {
    await button.click();
    await expect(card).toBeVisible();
    await expect(button).toHaveAttribute("aria-expanded", "true");
    await button.click();
    await expect(card).toBeHidden();
    await expect(button).toHaveAttribute("aria-expanded", "false");
  }

  await button.click();
  await expect(card).toContainText("Running");
  await page.keyboard.press("Escape");
  await expect(card).toBeHidden();

  await button.click();
  await expect(card).toBeVisible();
  // Right of the card, which opens in the top left corner
  await app.locator("body").click({ position: { x: 1000, y: 300 } });
  await expect(card).toBeHidden();
});

// LivebookWasm.Application stars the example notebooks of priv/notebooks/
test("the home page lists the example notebooks", async () => {
  const starred = app.locator("#starred-notebooks");
  for (const title of [
    "Charts with VegaLite",
    "Maps with MapLibre",
    "Processes and supervisors",
    "Tensors with Nx",
    "Nodes across tabs",
    "Testing and benchmarking",
  ])
    await expect(starred.getByText(title, { exact: true })).toBeVisible();
});

// Like a link of Livebook's opened in a new tab. Only the shell's iframe
// reaches the tab's VM, so the page opens a new shell, with a Livebook of its
// own, on its home page.
test("a link opened in a new tab opens a Livebook of its own", async () => {
  const livebook = await (await page.$("#livebook")).contentFrame();
  const opened = await context.newPage();
  watch(opened, "[opened tab] ");
  try {
    await opened.goto(
      new URL(`${tabPath(livebook)}/settings`, page.url()).href,
    );
    await opened.waitForFunction(() => window.livebookStarted, null, {
      timeout: 180_000,
    });
    await expect(
      opened.frameLocator("#livebook").locator(".phx-connected").first(),
    ).toBeVisible();
    expect(new URL(opened.url()).pathname).toBe("/");
    const openedLivebook = await (await opened.$("#livebook")).contentFrame();
    expect(tabPath(openedLivebook)).not.toBe(tabPath(livebook));
    expect(new URL(openedLivebook.url()).pathname).toBe(
      `${tabPath(openedLivebook)}/`,
    );
  } finally {
    await opened.close();
  }
});

test("a new notebook opens", async () => {
  await newNotebook().click();
  await expect(
    app.locator("[data-el-cell][data-type='code'] .cm-content").first(),
  ).toBeVisible();
});

// Whether Popcorn's card is open, once Livebook has had the time to take the
// focus back from the shell
const cardOpen = async () => {
  await page.waitForTimeout(300);
  return page.locator("#card").evaluate((card) => card.matches(":popover-open"));
};

// The new notebook's cell editor takes the focus back from the shell, which
// mustn't close the card
test("Popcorn's button toggles its card on a notebook", async () => {
  const button = page.locator("#popcorn");
  for (let i = 0; i < 2; i++) {
    await button.click();
    expect(await cardOpen()).toBe(true);
    await button.click();
    expect(await cardOpen()).toBe(false);
  }

  await button.click();
  await page.locator("#card p").first().click();
  expect(await cardOpen(), "after a click in the card").toBe(true);
  // An empty part of the notebook's page
  await app.locator("body").click({ position: { x: 1180, y: 600 } });
  expect(await cardOpen(), "after a click on Livebook's page").toBe(false);

  await button.click();
  await page.keyboard.press("Escape");
  expect(await cardOpen(), "after Escape in Livebook's page").toBe(false);
});

// Livebook opens its dashboard in a new tab, where a new shell would open its
// own Livebook on its home page, so the shell's Livebook opens it instead
test("the notebook's menu opens the dashboard in the tab's Livebook", async () => {
  const notebookUrl = (await (await page.$("#livebook")).contentFrame()).url();
  const pages = context.pages().length;
  await app.getByRole("button", { name: "open notebook menu" }).click();
  await app.getByRole("menuitem", { name: "See on Dashboard" }).click();
  // The session's process
  await expect(app.getByText("Livebook.Session.init/1").first()).toBeVisible();
  const livebook = await (await page.$("#livebook")).contentFrame();
  expect(new URL(livebook.url()).pathname).toContain(
    `${tabPath(livebook)}/dashboard/`,
  );
  expect(context.pages(), "the pages, with no new tab").toHaveLength(pages);

  // Back to the notebook, which only the iframe navigates to, unlike what
  // page.goBack() waits for
  await page.evaluate(() => history.back());
  await expect(
    app.locator("[data-el-cell][data-type='code'] .cm-content").first(),
  ).toBeVisible();
  expect((await (await page.$("#livebook")).contentFrame()).url()).toBe(
    notebookUrl,
  );
});

// The first evaluation starts the notebook's runtime, in a VM of its own. It
// boots from the package that the shell prefetched, so the server only
// confirms that the cached files are current.
test("a cell evaluates in the notebook's VM", async () => {
  await page.evaluate(() => window.notebookVmPrefetched);
  await evaluate("Enum.sum(1..10)", "55");
  await expect(page.locator("#notebook-vms")).toHaveText("1 running");

  // How much of each file of the package came from the network, as the VMs'
  // Popcorn workers record it. The VMs' threads' workers can be blocked.
  const files = [];
  for (const worker of page
    .workers()
    .filter((worker) => worker.url().endsWith("/worker.mjs"))) {
    const entries = await worker.evaluate(() =>
      performance
        .getEntriesByType("resource")
        .filter((entry) => entry.name.includes("/notebook_vm/otp/"))
        .map(({ name, transferSize, encodedBodySize }) => ({
          name,
          transferSize,
          encodedBodySize,
        })),
    );
    files.push(...entries);
  }
  expect(files.map((file) => file.name)).toContainEqual(
    expect.stringContaining("/notebook_vm/otp/lib/"),
  );
  const downloaded = files.filter(
    (file) => file.transferSize >= file.encodedBodySize,
  );
  expect(downloaded, "files downloaded again").toEqual([]);
});

test("the code runs in a browser VM other than Livebook's", async () => {
  const result = await evaluate(
    "{node(), :erlang.system_info(:system_architecture)}",
    "wasm32-unknown-emscripten",
  );
  runtimeNode = result.match(/runtime_[a-z2-7]+@popcorn/)?.[0];
  expect(runtimeNode, `the runtime's node in ${result}`).toBeDefined();
  const booted = logs.find((line) =>
    line.includes(`Runtime VM ${runtimeNode} booted`),
  );
  console.log(
    `  ${runtimeNode} ${booted?.match(/booted in \d+ ms/)?.[0] ?? ""}`,
  );
});

// The notebook's VM loads Elixir's and Erlang's docs after boot, where
// Code.fetch_docs/1, which Livebook's code completion and docs use, finds
// them. It fetches them on the shell's page, which prefetched them.
test("the notebook's VM gets Elixir's and Erlang's docs", async () => {
  await evaluate(
    "Enum.find_value(1..100, fn _ -> match?({:docs_v1, _, _, _, _, _, _}, Code.fetch_docs(:lists)) or (Process.sleep(100) && nil) end) && " +
      "{:docs, elem(Code.fetch_docs(Enum), 0), elem(Code.fetch_docs(:lists), 0)}",
    "{:docs, :docs_v1, :docs_v1}",
  );
  const downloaded = await page.evaluate(() =>
    performance
      .getEntriesByType("resource")
      .filter((entry) => entry.name.endsWith("/notebook_vm/docs.tar"))
      .map((entry) => entry.transferSize >= entry.encodedBodySize),
  );
  expect(downloaded, "the prefetch's and the VM's downloads").toEqual([
    true,
    false,
  ]);
});

// Kino, a Mix package and a rebar3-only one, which Livebook doesn't depend on,
// Req, with Mint, whose Erlang code includes OTP's headers, parse_trans,
// whose parse transforms use syntax_tools, ranch, built with erlang.mk, which
// has an .app file instead of an .app.src one, and seshat, which includes its
// headers relative to the package
test("Mix.install downloads and compiles packages", async () => {
  test.setTimeout(660_000);
  await evaluate(
    `Mix.install([{:kino, "~> 0.18.0"}, {:nimble_csv, "~> 1.2"}, {:jsone, "~> 1.9"}, :req, {:parse_trans, "~> 3.4"}, {:ranch, "~> 2.2"}, {:seshat, "~> 1.0"}])`,
    ":ok",
    600_000,
  );
});

test("Kino.JS outputs render in their iframes", async () => {
  await run(`Kino.HTML.new("<p>Rendered by Kino.JS</p>")`);
  // Livebook keeps outputs' iframes outside the cells, over their placeholders
  const frame = app
    .locator("iframe[src*='/iframe/v6.html']")
    .first()
    .contentFrame();
  await expect(frame.getByText("Rendered by Kino.JS")).toBeVisible({
    timeout: 120_000,
  });

  // A click on the output, right of Popcorn's card, closes the card
  await page.locator("#popcorn").click();
  expect(await cardOpen()).toBe(true);
  const box = await app
    .locator("iframe[src*='/iframe/v6.html']")
    .first()
    .boundingBox();
  await page.mouse.click(box.x + box.width - 20, box.y + box.height / 2);
  expect(await cardOpen(), "after a click on the output").toBe(false);
});

// Binary payloads go as binary WebSocket frames: the initial one and "pong"
// from the VM, "ping" to it. On one line, as the editor indents new lines and
// Enter could pick a completion.
test("Kino.JS.Live sends and receives binary payloads", async () => {
  const js =
    "export function init(ctx, payload) { " +
    "const show = ([info, buffer]) => { ctx.root.innerText = info.message + ':' + new Uint8Array(buffer).join(','); }; " +
    "show(payload); ctx.handleEvent('pong', show); ctx.pushEvent('ping', [{}, new Uint8Array([3]).buffer]); }";
  await run(
    "defmodule BinaryKino do use Kino.JS; use Kino.JS.Live; " +
      "def new, do: Kino.JS.Live.new(__MODULE__, nil); " +
      `def handle_connect(ctx), do: {:ok, {:binary, %{message: "hello"}, <<1, 2>>}, ctx}; ` +
      `def handle_event("ping", {:binary, _info, bytes}, ctx) do broadcast_event(ctx, "pong", {:binary, %{message: "pong"}, <<1, 2, bytes::binary>>}); {:noreply, ctx} end; ` +
      `asset "main.js", do: "${js}" end; BinaryKino.new()`,
  );
  await expect
    .poll(async () => {
      const outputs = page
        .frames()
        .filter((frame) => frame.url().includes("/iframe/v6.html"));
      const texts = await Promise.all(
        outputs.map((frame) =>
          frame
            .locator("body")
            .innerText()
            .catch(() => ""),
        ),
      );
      return texts.join("\n");
    })
    .toContain("pong:1,2,3");
});

// Each tab runs a Livebook of its own, under a path with a Service Worker of
// its own. Only the first tab's VM has its notebook.
test("each tab's requests and navigations go to its own VM", async () => {
  const other = await context.newPage();
  watch(other, "[other tab] ");
  try {
    await other.goto("/");
    await other.waitForFunction(() => window.livebookStarted, null, {
      timeout: 180_000,
    });
    await expect(
      other.frameLocator("#livebook").locator(".phx-connected").first(),
    ).toBeVisible();

    // Whether a notebook is under the frame's tab's path
    const found = (frame, path) =>
      frame.evaluate(async (path) => {
        const base =
          window.LIVEBOOK_BASE_URL_PATH ?? window.parent.LIVEBOOK_BASE_URL_PATH;
        const resp = await fetch(base + path);
        return resp.ok && !resp.redirected;
      }, path);

    const notebook = sessionFrame();
    const kinoOutput = page
      .frames()
      .find((frame) => frame.url().includes("/iframe/v6.html"));
    const otherTab = await (await other.$("#livebook")).contentFrame();
    const path = new URL(notebook.url()).pathname.slice(
      tabPath(notebook).length,
    );

    expect({
      notebook: await found(notebook, path),
      output: await found(kinoOutput, path),
      otherTab: await found(otherTab, path),
    }).toEqual({ notebook: true, output: true, otherTab: false });

    // The other tab was opened last, which mustn't matter
    const url = notebook.url();
    const reloaded = page.waitForEvent(
      "framenavigated",
      (frame) => frame === notebook,
    );
    await notebook.evaluate(() => location.reload());
    await reloaded;
    await expect(
      app.locator(".phx-connected [data-el-cell]").first(),
    ).toBeVisible();
    expect(notebook.url()).toBe(url);
  } finally {
    await other.close();
  }
});

test("installed packages run", async () => {
  await evaluate(
    `{NimbleCSV.RFC4180.parse_string("a,b\\n1,2\\n"), :jsone.decode("[1,2]")}`,
    `{[["1", "2"]], [1, 2]}`,
  );
});

// With Popcorn's adapter, which it installs once Mix.install/2 has started Req.
// jsDelivr compresses the JSON, which the browser decompresses.
test("Req requests with the browser's fetch()", async () => {
  await evaluate(
    `resp = Req.get!("https://cdn.jsdelivr.net/npm/vega-datasets@2/data/gapminder.json")\n` +
      `{Req.default_options()[:adapter] == Popcorn.Fetch.adapter(), resp.status, is_list(resp.body)}`,
    "{true, 200, true}",
  );
});

// Mix.install/2 installs once per VM
test("another notebook installs other packages in a VM of its own", async () => {
  test.setTimeout(660_000);
  const session = sessionFrame();
  await session.goto(new URL(`${tabPath(session)}/new`, page.url()).href);
  await expect(cell.locator(".cm-content")).toBeVisible();

  const result = await evaluate(
    `Mix.install([{:jason, "~> 1.4"}])\n{node(), Jason.encode!(%{installed: true})}`,
    "@popcorn",
    600_000,
  );
  expect(result, "the first notebook's VM").not.toContain(runtimeNode);
  expect(result).toContain(`{\\"installed\\":true}`);
  runtimeNode = result.match(/runtime_[a-z2-7]+@popcorn/)[0];
  await expect(page.locator("#notebook-vms")).toHaveText("2 running");
});

test("reconnecting replaces the notebook's VM", async () => {
  await app.locator("[data-el-runtime-info-toggle]").click();
  await app
    .locator("[data-el-runtime-info]")
    .getByRole("button", { name: "Reconnect" })
    .click();

  // A result that the cell's previous output doesn't have
  const result = await evaluate("{:reconnected, node()}", ":reconnected");
  expect(result, "the same VM").not.toContain(runtimeNode);

  const stopped = `Runtime VM ${runtimeNode} stopped`;
  await expect
    .poll(() => logs.some((line) => line.includes(stopped)), {
      message: `${runtimeNode} kept running`,
      timeout: 10_000,
    })
    .toBe(true);
  await expect(page.locator("#notebook-vms")).toHaveText("2 running");
});

// The tabs' VMs are nodes that connect across tabs, see public/distribution.js:
// a notebook in another tab calls this notebook's VM, with this tab's cookie
// only, and the VM learns when the other tab closes
test("notebooks connect to other tabs' VMs", async () => {
  const [, here, cookie] = (
    await evaluate("{:here, node(), Node.get_cookie()}", ":here")
  ).match(/(runtime_[a-z2-7]+@popcorn), :"?(c_[\w-]+)/);
  const other = await context.newPage();
  watch(other, "[other tab] ");
  let there;
  try {
    await other.goto("/");
    await other.waitForFunction(() => window.livebookStarted, null, {
      timeout: 180_000,
    });
    const otherApp = other.frameLocator("#livebook");
    await expect(otherApp.locator(".phx-connected").first()).toBeVisible();
    await otherApp
      .getByText("New notebook")
      .filter({ visible: true })
      .first()
      .click();
    const otherNotebook = notebook(other);
    await expect(otherNotebook.cell.locator(".cm-content")).toBeVisible();

    const result = await otherNotebook.evaluate(
      `{node(), Node.connect(:"${here}"), Node.set_cookie(:"${here}", :"${cookie}"), ` +
        `Node.connect(:"${here}"), :erpc.call(:"${here}", Node, :self, [])}`,
      `false, true, true, :${here}}`,
    );
    there = result.match(/runtime_[a-z2-7]+@popcorn/)[0];
    await evaluate(
      `{:connected, :"${there}" in Node.list()}`,
      "{:connected, true}",
    );
  } finally {
    await other.close();
  }

  await evaluate(
    `Enum.find_value(1..100, fn _ -> if :"${there}" in Node.list(), do: (Process.sleep(100); nil), else: :disconnected end)`,
    ":disconnected",
  );
});

// livebook.dev checks that the Livebook address set there is a Livebook's
// with this request, from its own pages
test("livebook.dev recognizes the example as a Livebook", async () => {
  const resp = await page.request.get("/public/health");
  expect(resp.headers()["access-control-allow-origin"]).toBe("*");
  expect(await resp.json()).toEqual({ application: "livebook" });
});

// Where livebook.dev's "Run in Livebook" badges send people, with the
// example's address as their Livebook's
test("a link opens a notebook from GitHub", async () => {
  const notebook =
    "https://github.com/livebook-dev/livebook/blob/main/lib/livebook/notebook/learn/intro_to_livebook.livemd";
  const linked = await context.newPage();
  watch(linked, "[linked tab] ");
  try {
    await linked.goto(`/import?url=${encodeURIComponent(notebook)}`);
    await linked.waitForFunction(() => window.livebookStarted, null, {
      timeout: 180_000,
    });
    await linked.waitForFunction(
      () =>
        document
          .querySelector("#livebook")
          ?.contentWindow.location.pathname.includes("/sessions/"),
      null,
      { timeout: 60_000 },
    );
    await expect(
      linked.frameLocator("#livebook").getByText("Welcome to Livebook").first(),
    ).toBeVisible();

    // So that reloading doesn't open the link again
    await linked.waitForFunction(
      () => location.pathname + location.search === "/",
      null,
      { timeout: 10_000 },
    );
  } finally {
    await linked.close();
  }
});

// LiveView uploads the file in binary WebSocket frames
test("a notebook uploads through the form", async () => {
  // Livebook's states while the frame opens another page, which mustn't show
  // the previous page's leaving as a lost connection
  await page.evaluate(() => {
    const status = document.querySelector("#status");
    window.statusStates = [];
    new MutationObserver(() =>
      window.statusStates.push(status.dataset.state),
    ).observe(status, { attributeFilter: ["data-state"] });
  });
  const livebook = await (await page.$("#livebook")).contentFrame();
  await livebook.goto(
    new URL(`${tabPath(livebook)}/open/upload`, page.url()).href,
  );
  // Before the LiveView connects, nothing handles the file
  await expect(app.locator(".phx-connected #upload-file-form")).toBeVisible();
  await app.locator("#upload-file-form input[type=file]").setInputFiles({
    name: "uploaded.livemd",
    mimeType: "text/plain",
    buffer: Buffer.from(
      "# Uploaded through the form\n\n```elixir\n1 + 1\n```\n",
    ),
  });
  await app
    .locator("#upload-file-form")
    .getByRole("button", { name: "Import" })
    .click();
  await expect(
    app.getByText("Uploaded through the form").first(),
  ).toBeVisible();
  // The page that the frame opened connected too
  await expect(page.locator("#status")).toHaveAttribute("data-state", "ready");
  expect(await page.evaluate(() => window.statusStates)).not.toContain("error");
});

// Popcorn's card says that Livebook runs only once its page has loaded and
// connected. The relay frame, which Livebook's pages load through, and
// Livebook's pages load scripts from the server, which can stop after
// Livebook's VM has started.
for (const [frame, request] of [
  ["the relay frame", 1],
  ["Livebook's page", 2],
]) {
  test(`the card shows that Livebook's page didn't load, without ${frame}'s script`, async () => {
    const failing = await context.newPage();
    watch(failing, "[failing tab] ");
    let requests = 0;
    await failing.route("**/popcorn-frame.js", (route) =>
      ++requests === request ? route.abort() : route.continue(),
    );
    try {
      await failing.goto("/");
      await expect(failing.locator("#status")).toHaveAttribute(
        "data-state",
        "error",
        { timeout: 180_000 },
      );
      await expect(failing.locator("#status-text")).toHaveText(
        "Its page didn't load: /popcorn-frame.js failed",
      );
      await expect(failing.locator("#card")).toBeVisible();
      // The loader stays, without its animation
      await expect(failing.locator("#loader")).toHaveAttribute(
        "data-state",
        "error",
      );
    } finally {
      await failing.close();
    }
  });
}
