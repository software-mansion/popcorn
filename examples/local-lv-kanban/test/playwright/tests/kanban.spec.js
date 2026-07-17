// @ts-check
const { test, expect } = require("@playwright/test");
const h = require("./helpers");

let seq = 0;
const uniqueName = (prefix) => `${prefix} ${Date.now().toString(36)}-${seq++}`;

const SEEDED = ["To Do", "In Progress", "Done"];

test.describe("board index + lifecycle", () => {
  test("create a board, it appears in the index and opens with seeded columns", async ({ page }) => {
    const name = uniqueName("Lifecycle");
    const url = await h.createBoard(page, name);

    // The local live view heading shows the board's own name.
    await expect(page.getByRole("heading", { name, exact: true })).toBeVisible();

    // Seeded columns render in the WASM local live view.
    expect(await h.columnNames(page)).toEqual(SEEDED);

    // The board is listed on the index and links back to itself.
    await page.goto("/");
    const link = page.getByRole("link", { name, exact: true });
    await expect(link).toBeVisible();
    await link.click();
    await page.waitForURL(url);
    await h.waitForBoard(page);
    expect(await h.columnNames(page)).toEqual(SEEDED);
  });
});

test.describe("columns", () => {
  test("add a column (optimistic) and it persists across reload", async ({ page }) => {
    await h.createBoard(page, uniqueName("AddCol"));
    await h.addColumn(page, "Backlog");
    expect(await h.columnNames(page)).toEqual([...SEEDED, "Backlog"]);

    await page.reload();
    await h.waitForBoard(page);
    expect(await h.columnNames(page)).toEqual([...SEEDED, "Backlog"]);
  });

  test("the add-column input clears after a successful add", async ({ page }) => {
    await h.createBoard(page, uniqueName("ColInput"));
    await h.addColumn(page, "Backlog");
    await expect(page.locator("form[phx-submit='add_column'] input[name=name]")).toHaveValue("");
  });

  test("remove a column (optimistic) and it stays gone after reload", async ({ page }) => {
    await h.createBoard(page, uniqueName("RmCol"));
    await h.removeColumn(page, "In Progress");
    expect(await h.columnNames(page)).toEqual(["To Do", "Done"]);

    await page.reload();
    await h.waitForBoard(page);
    expect(await h.columnNames(page)).toEqual(["To Do", "Done"]);
  });
});

test.describe("tasks", () => {
  test("add a task with a description and it persists", async ({ page }) => {
    await h.createBoard(page, uniqueName("AddTask"));
    await h.addTask(page, "To Do", "Write tests", "cover drag&drop");

    await expect(h.taskCard(page, "Write tests")).toBeVisible();
    await expect(page.getByText("cover drag&drop")).toBeVisible();
    expect(await h.taskTextsIn(page, "To Do")).toEqual(["Write tests"]);

    await page.reload();
    await h.waitForBoard(page);
    await expect(h.taskCard(page, "Write tests")).toBeVisible();
  });

  test("remove a task (optimistic) and it stays gone after reload", async ({ page }) => {
    await h.createBoard(page, uniqueName("RmTask"));
    await h.addTask(page, "To Do", "Throwaway");
    await h.removeTask(page, "Throwaway");
    expect(await h.taskTextsIn(page, "To Do")).toEqual([]);

    await page.reload();
    await h.waitForBoard(page);
    await expect(h.taskCard(page, "Throwaway")).toHaveCount(0);
  });

  test("the modal closes after adding a task", async ({ page }) => {
    await h.createBoard(page, uniqueName("Modal"));
    await h.addTask(page, "To Do", "Quick");
    await expect(page.locator("input[name=text]")).toHaveCount(0);
  });
});

test.describe("drag & drop", () => {
  test("reorder tasks within a column", async ({ page }) => {
    await h.createBoard(page, uniqueName("DragReorder"));
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
    await h.createBoard(page, uniqueName("DragMove"));
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
      const url = await h.createBoard(A, uniqueName("Realtime"));
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
    await h.createBoard(page, uniqueName("Rollback"));
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
    await h.createBoard(page, uniqueName("PushErr"));
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

    // add_column applies optimistically in WASM, then the failed push triggers
    // handle_push_error, restoring the last authoritative board.
    await h.submitColumn(page, "Ghost");
    await page.waitForFunction(() => window.__llvTestSawGhost, null, { timeout: 15_000 });
    await expect(h.columnByName(page, "Ghost")).toHaveCount(0, { timeout: 15_000 });
    await expect(h.columns(page)).toHaveCount(3);
  });

  test("an optimistic remove snaps back when the socket is down", async ({ page }) => {
    await h.createBoard(page, uniqueName("PushErrDom"));
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
