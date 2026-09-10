// @ts-check
const { test, expect } = require("@playwright/test");
const h = require("./helpers");

const SEEDED = ["To Do", "In Progress", "Done"];

test.describe("board index + lifecycle", () => {
  test("create a board, it opens with seeded columns and shows up under recents", async ({ page }) => {
    const url = await h.createBoard(page);

    // The local live view heading shows the board's own name.
    await expect(page.getByRole("heading", { name: "Untitled board", exact: true })).toBeVisible();

    // Seeded columns render in the Wasm local live view.
    expect(await h.columnNames(page)).toEqual(SEEDED);

    // Opening the board recorded it in localStorage; the index renders it as a
    // recent board linking back to itself.
    await page.goto("/");
    const link = page.getByRole("link", { name: "Untitled board", exact: true });
    await expect(link).toBeVisible();
    await link.click();
    await page.waitForURL(url);
    await h.waitForBoard(page);
    expect(await h.columnNames(page)).toEqual(SEEDED);
  });

  test("rename the board; it persists and recents pick the new name up", async ({ page }) => {
    const url = await h.createBoard(page);

    await page.getByTitle("Rename board").click();
    const input = page.locator("form[phx-submit='rename_board'] input[name=name]");
    await input.fill("Renamed board");
    await page.getByRole("button", { name: "Save", exact: true }).click();
    await expect(page.getByRole("heading", { name: "Renamed board", exact: true })).toBeVisible();

    // Server-persisted.
    await page.reload();
    await h.waitForBoard(page);
    await expect(page.getByRole("heading", { name: "Renamed board", exact: true })).toBeVisible();

    // Recents store only ids, so the index shows the fresh DB name.
    await page.goto("/");
    await expect(page.getByRole("link", { name: "Renamed board", exact: true })).toBeVisible();

    // An invalid name (too long for the server) rolls back to the current one.
    await page.goto(url);
    await h.waitForBoard(page);
    await page.getByTitle("Rename board").click();
    await input.fill("x".repeat(300));
    await page.getByRole("button", { name: "Save", exact: true }).click();
    // The form closes as soon as the submit is applied optimistically...
    await expect(input).toHaveCount(0);
    // ...and the server's rejection then restores the persisted name.
    await expect(page.getByRole("heading", { name: "Renamed board", exact: true })).toBeVisible({
      timeout: 15_000,
    });
  });

  test("generate a sample board pre-filled with tasks", async ({ page }) => {
    await page.goto("/");
    await page.getByRole("button", { name: "Generate sample board" }).click();
    await page.waitForURL(/\/boards\/[0-9a-f-]+$/);
    await h.waitForBoard(page);

    expect(await h.tasks(page).count()).toBeGreaterThan(0);
  });
});

test.describe("columns", () => {
  test("add a column (optimistic) and it persists across reload", async ({ page }) => {
    await h.createBoard(page);
    await h.addColumn(page, "Backlog");
    expect(await h.columnNames(page)).toEqual([...SEEDED, "Backlog"]);

    await page.reload();
    await h.waitForBoard(page);
    expect(await h.columnNames(page)).toEqual([...SEEDED, "Backlog"]);
  });

  test("the add-column input clears after a successful add", async ({ page }) => {
    await h.createBoard(page);
    await h.addColumn(page, "Backlog");
    await expect(page.locator("form[phx-submit='add_column'] input[name=name]")).toHaveValue("");
  });

  test("remove a column (optimistic) and it stays gone after reload", async ({ page }) => {
    await h.createBoard(page);
    await h.removeColumn(page, "In Progress");
    expect(await h.columnNames(page)).toEqual(["To Do", "Done"]);

    await page.reload();
    await h.waitForBoard(page);
    expect(await h.columnNames(page)).toEqual(["To Do", "Done"]);
  });
});

test.describe("tasks", () => {
  test("add a task with a description and it persists", async ({ page }) => {
    await h.createBoard(page);
    await h.addTask(page, "To Do", "Write tests", "cover drag&drop");

    await expect(h.taskCard(page, "Write tests")).toBeVisible();
    await expect(page.getByText("cover drag&drop")).toBeVisible();
    expect(await h.taskTextsIn(page, "To Do")).toEqual(["Write tests"]);

    await page.reload();
    await h.waitForBoard(page);
    await expect(h.taskCard(page, "Write tests")).toBeVisible();
  });

  test("remove a task (optimistic) and it stays gone after reload", async ({ page }) => {
    await h.createBoard(page);
    await h.addTask(page, "To Do", "Throwaway");
    await h.removeTask(page, "Throwaway");
    expect(await h.taskTextsIn(page, "To Do")).toEqual([]);

    await page.reload();
    await h.waitForBoard(page);
    await expect(h.taskCard(page, "Throwaway")).toHaveCount(0);
  });

  test("the modal closes after adding a task", async ({ page }) => {
    await h.createBoard(page);
    await h.addTask(page, "To Do", "Quick");
    await expect(page.locator("input[name=text]")).toHaveCount(0);
  });
});

test.describe("drag & drop", () => {
  test("reorder tasks within a column", async ({ page }) => {
    await h.createBoard(page);
    await h.addTask(page, "To Do", "Alpha");
    await h.addTask(page, "To Do", "Beta");
    expect(await h.taskTextsIn(page, "To Do")).toEqual(["Alpha", "Beta"]);

    await h.dragTaskBeforeTask(page, "Beta", "Alpha");
    await expect.poll(() => h.taskTextsIn(page, "To Do")).toEqual(["Beta", "Alpha"]);

    // order is server-persisted
    await page.reload();
    await h.waitForBoard(page);
    expect(await h.taskTextsIn(page, "To Do")).toEqual(["Beta", "Alpha"]);
  });

  test("move a task across columns", async ({ page }) => {
    await h.createBoard(page);
    await h.addTask(page, "To Do", "Mover");
    expect(await h.taskTextsIn(page, "To Do")).toEqual(["Mover"]);

    await h.dragTaskToColumn(page, "Mover", "Done");
    await expect.poll(() => h.taskTextsIn(page, "Done")).toEqual(["Mover"]);
    expect(await h.taskTextsIn(page, "To Do")).toEqual([]);

    await page.reload();
    await h.waitForBoard(page);
    expect(await h.taskTextsIn(page, "Done")).toEqual(["Mover"]);
    expect(await h.taskTextsIn(page, "To Do")).toEqual([]);
  });
});

test.describe("realtime collaboration (two clients)", () => {
  test("adds, moves and removes propagate to other viewers", async ({ browser }) => {
    const ctxA = await browser.newContext();
    const ctxB = await browser.newContext();
    const A = await ctxA.newPage();
    const B = await ctxB.newPage();

    try {
      const url = await h.createBoard(A);
      await h.openBoard(B, url);

      // add column on A -> visible on B
      await h.addColumn(A, "Backlog");
      await expect(h.columnByName(B, "Backlog")).toBeVisible({ timeout: 20_000 });

      // add tasks on A -> visible on B
      await h.addTask(A, "To Do", "Shared");
      await h.addTask(A, "To Do", "Second");
      await expect(h.taskCard(B, "Shared")).toBeVisible({ timeout: 20_000 });
      await expect(h.taskCard(B, "Second")).toBeVisible({ timeout: 20_000 });

      // drag on A -> reorder visible on B
      await h.dragTaskBeforeTask(A, "Second", "Shared");
      await expect.poll(() => h.taskTextsIn(B, "To Do"), { timeout: 20_000 }).toEqual(["Second", "Shared"]);

      // remove on A -> gone on B
      await h.removeTask(A, "Shared");
      await expect(h.taskCard(B, "Shared")).toHaveCount(0, { timeout: 20_000 });
    } finally {
      await ctxA.close();
      await ctxB.close();
    }
  });
});

test.describe("optimistic rollback", () => {
  test("a server-rejected edit rolls back and never persists", async ({ page }) => {
    await h.createBoard(page);
    await expect(h.columns(page)).toHaveCount(3);

    // The server validates column name length (<= 255). A 300-char name is added
    // optimistically in the browser, then rejected on the server, which re-pushes
    // the authoritative board — rolling the optimistic column back to 3.
    await h.submitColumn(page, "x".repeat(300));
    await expect(h.columns(page)).toHaveCount(3, { timeout: 15_000 });

    await page.reload();
    await h.waitForBoard(page);
    await expect(h.columns(page)).toHaveCount(3);
  });
});

test.describe("push failure (handle_push_error)", () => {
  test("an optimistic add snaps back when the socket is down", async ({ page }) => {
    await h.createBoard(page);
    await expect(h.columns(page)).toHaveCount(3);

    // Kill the host websocket: push_server_event rejects with "no connection".
    await page.evaluate(() => window.liveSocket.disconnect());

    // The rollback can remove the optimistic column within milliseconds of it
    // rendering — too fast for locator polling — so record its appearance with
    // a MutationObserver instead.
    await page.evaluate(() => {
      window.__llvTestSawGhost = false;
      new MutationObserver(() => {
        if (document.body.textContent.includes("Ghost")) window.__llvTestSawGhost = true;
      }).observe(document.body, { childList: true, subtree: true, characterData: true });
    });

    // add_column applies optimistically in Wasm, then the failed push triggers
    // handle_push_error, restoring the last authoritative board.
    await h.submitColumn(page, "Ghost");
    await page.waitForFunction(() => window.__llvTestSawGhost, null, { timeout: 15_000 });
    await expect(h.columnByName(page, "Ghost")).toHaveCount(0, { timeout: 15_000 });
    await expect(h.columns(page)).toHaveCount(3);
  });

  test("an optimistic remove snaps back when the socket is down", async ({ page }) => {
    await h.createBoard(page);
    await expect(h.columns(page)).toHaveCount(3);

    // Kill the host websocket: the remove's push_server_event rejects with
    // "no connection".
    await page.evaluate(() => window.liveSocket.disconnect());

    // The rollback can re-render the column within milliseconds of the
    // optimistic removal — too fast for locator polling — so record the
    // disappearance with a MutationObserver instead.
    await page.evaluate(() => {
      window.__llvTestSawRemoval = false;
      new MutationObserver(() => {
        const columns = document.querySelectorAll("[phx-dragover='drag_over_column']");
        const present = [...columns].some((c) => c.textContent.includes("In Progress"));
        if (!present) window.__llvTestSawRemoval = true;
      }).observe(document.body, { childList: true, subtree: true, characterData: true });
    });

    // The local handler removes the column optimistically, then its failed
    // push_server_event triggers handle_push_error, restoring the board.
    await h.columnByName(page, "In Progress").getByTitle("Remove column").click();
    await page.waitForFunction(() => window.__llvTestSawRemoval, null, { timeout: 15_000 });
    await expect(h.columnByName(page, "In Progress")).toHaveCount(1, { timeout: 15_000 });
    await expect(h.columns(page)).toHaveCount(3);
  });
});

test.describe("channel crash recovery", () => {
  test("a crashed local view remounts and stays usable", async ({ page }) => {
    await h.createBoard(page);
    expect(await h.columnNames(page)).toEqual(SEEDED);

    // Push an event no handle_event clause matches: the WASM channel process
    // crashes, the dispatcher forwards phx_error, and the stock phoenix.js
    // channel schedules a rejoin — a fresh mount from the stored config.
    const pushed = await page.evaluate(() => {
      const roots = Object.values(window.liveSocket.roots ?? {});
      const view = roots.find((v) => v.el.hasAttribute("data-pop-root"));
      if (!view) return false;
      view.channel.push("event", { type: "hook", event: "__crash_for_test__", value: {} });
      return true;
    });
    expect(pushed).toBe(true);

    // The forwarded phx_error puts the view in error state (phx-error class),
    // and the rejoin — phoenix.js' first backoff, ~1s — clears it on remount.
    // Interacting inside the error window would race the dying channel, so
    // gate on the full cycle before acting.
    const container = page.locator("[data-pop-root]");
    await expect(container).toHaveClass(/phx-error/, { timeout: 15_000 });
    await expect(container).not.toHaveClass(/phx-error/, { timeout: 15_000 });

    // The remounted view renders the board again and keeps working end to end.
    expect(await h.columnNames(page)).toEqual(SEEDED);
    await h.addColumn(page, "After crash");
    await expect(h.columnByName(page, "After crash")).toBeVisible();
  });
});

test.describe("live navigation away (replaceMain)", () => {
  test("navigating to the index tears the local view down; coming back remounts", async ({
    page,
  }) => {
    const browserLog = [];
    page.on("console", (m) => browserLog.push(`${m.type()}: ${m.text()}`));
    page.on("pageerror", (e) => browserLog.push(`pageerror: ${e.message}`));

    await h.createBoard(page);
    expect(await h.columnNames(page)).toEqual(SEEDED);

    // A full page load would tear everything down trivially — plant a flag a
    // reload would wipe to prove the navigation below is live (replaceMain).
    await page.evaluate(() => {
      window.__llvLiveNavProbe = true;
    });
    await page.getByRole("link", { name: "All boards" }).click();
    try {
      await expect(page.getByRole("heading", { name: "Kanban boards" })).toBeVisible({
        timeout: 15_000,
      });
    } catch (err) {
      console.log("URL at failure:", page.url());
      const state = await page.evaluate(() => {
        const main = document.querySelector("[data-phx-main]");
        const ls = window.liveSocket;
        return {
          socketConnected: ls.isConnected(),
          domMain: main && { id: main.id, children: main.children.length },
          lsMain: ls.main && {
            id: ls.main.id,
            elId: ls.main.el?.id,
            attached: ls.main.el?.isConnected,
            destroyed: ls.main.destroyed,
          },
          roots: Object.keys(ls.roots ?? {}),
          popRoots: Array.from(document.querySelectorAll("[data-pop-root]")).map((e) => ({
            id: e.id,
            inMain: !!e.closest("[data-phx-main]"),
          })),
          headingAnywhere: !!Array.from(document.querySelectorAll("h1")).find((h) =>
            h.textContent.includes("Kanban boards"),
          ),
        };
      });
      console.log("state at failure:", JSON.stringify(state, null, 2));
      console.log("browser log:\n" + browserLog.join("\n"));
      throw err;
    }
    expect(await page.evaluate(() => window.__llvLiveNavProbe)).toBe(true);

    // replaceMain MOVES data-phx-sticky elements into the new page instead
    // of discarding them; the new main's join patch then discards the moved
    // husk, which is what destroys the client View and sends the channel
    // leave (stock teardown — Views.unmount deliberately doesn't destroy
    // the View, see its comment). Both must converge: no live-view zombie
    // in the socket's roots, no stray [data-pop-root] husk in the DOM.
    await expect
      .poll(async () =>
        page.evaluate(() => ({
          zombies: Object.values(window.liveSocket.roots ?? {})
            .filter((v) => v.el?.hasAttribute?.("data-pop-root"))
            .map((v) => v.id),
          husks: Array.from(document.querySelectorAll("[data-pop-root]")).map((e) => e.id),
        })),
      )
      .toEqual({ zombies: [], husks: [] });

    // Going back remounts a fresh incarnation of the same view id on the
    // same runtime, and it works end to end.
    await page.goBack();
    await h.waitForBoard(page);
    expect(await h.columnNames(page)).toEqual(SEEDED);
    await h.addColumn(page, "After live nav");
    await expect(h.columnByName(page, "After live nav")).toBeVisible();
  });
});
