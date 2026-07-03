// @ts-check
const { expect } = require("@playwright/test");

// Locators keyed off the kanban's phx-* bindings (stable, structural).
const COLUMN = "[phx-dragover='drag_over_column']";
const TASK = "[phx-dragstart='drag_start']";

const columns = (page) => page.locator(COLUMN);
const tasks = (page) => page.locator(TASK);

// A column located by its header name (names must stay distinct from task text).
const columnByName = (page, name) => columns(page).filter({ hasText: name });

// A task card located by its text.
const taskCard = (page, text) => tasks(page).filter({ hasText: text });

/** Wait until the WASM local live view has booted and rendered the board. */
async function waitForBoard(page) {
  // The heading now shows the board's own name, so the first rendered column is
  // the name-agnostic boot signal.
  await expect(columns(page).first()).toBeVisible({ timeout: 60_000 });
}

/** Create a board from the index and land on its page. Returns the board URL. */
async function createBoard(page, name) {
  await page.goto("/");
  await expect(page.getByRole("heading", { name: "Kanban boards" })).toBeVisible();
  await page.fill("input[name=name]", name);
  await page.getByRole("button", { name: "Create", exact: true }).click();
  await page.waitForURL(/\/boards\/[0-9a-f-]+$/);
  await waitForBoard(page);
  return page.url();
}

/** Open an existing board URL and wait for it to render. */
async function openBoard(page, url) {
  await page.goto(url);
  await waitForBoard(page);
}

/** Add a column via the "Add column" form (phx-target composes default+server). */
async function addColumn(page, name) {
  await page.fill("form[phx-submit='add_column'] input[name=name]", name);
  await page.locator("form[phx-submit='add_column'] button[type=submit]").click();
  await expect(columnByName(page, name)).toBeVisible();
}

/** Submit the add-column form without waiting for the column to stick (rollback). */
async function submitColumn(page, name) {
  await page.fill("form[phx-submit='add_column'] input[name=name]", name);
  await page.locator("form[phx-submit='add_column'] button[type=submit]").click();
}

/** Open the modal for a column and add a task. */
async function addTask(page, columnName, text, description = "") {
  await columnByName(page, columnName)
    .getByRole("button", { name: "+ Add task" })
    .click();
  await expect(page.locator("input[name=text]")).toBeVisible();
  await page.fill("input[name=text]", text);
  if (description) await page.fill("textarea[name=description]", description);
  await page.getByRole("button", { name: "Add task", exact: true }).click();
  await expect(taskCard(page, text)).toBeVisible();
}

async function removeColumn(page, name) {
  await columnByName(page, name).getByTitle("Remove column").click();
  await expect(columnByName(page, name)).toHaveCount(0);
}

async function removeTask(page, text) {
  await taskCard(page, text).getByTitle("Remove task").click();
  await expect(taskCard(page, text)).toHaveCount(0);
}

/** Ordered task texts within a column (by header name). */
async function taskTextsIn(page, columnName) {
  return columnByName(page, columnName).locator(TASK).allInnerTexts().then((arr) =>
    arr.map((t) => t.trim().split("\n")[0].trim()),
  );
}

/** Ordered column header names. */
async function columnNames(page) {
  return columns(page).evaluateAll((els) =>
    els.map((e) => e.textContent.trim().split("\n")[0].trim().replace(/\s*\(\d+\)$/, "")),
  );
}

// --- HTML5 drag & drop --------------------------------------------------------
// The kanban uses native draggable + drag events (wired by the LLV custom-binding
// JS that listens on window). Playwright's mouse-based dragTo does not fire those,
// so we dispatch real DragEvents (with a DataTransfer) and let the bindings run.
// Sequence: dragstart(src) -> dragover(target) -> dragend(src). The local view
// processes them in order (waits give the WASM round-trips time to land).

// Dispatch a drag event on the first element matching `selector` whose text
// contains `text`. The selector MUST be specific (TASK vs COLUMN): a column is an
// ancestor of its task cards, so a combined selector would match the column first
// and e.g. fire dragstart on an element that has no phx-dragstart binding.
async function _dispatchOn(page, selector, text, type, coords) {
  await page.evaluate(
    ({ selector, text, type, coords }) => {
      const el = [...document.querySelectorAll(selector)].find((e) =>
        e.textContent.includes(text),
      );
      if (!el) throw new Error(`drag: no ${selector} containing "${text}"`);
      const init = { bubbles: true, cancelable: true, dataTransfer: new DataTransfer() };
      if (coords) Object.assign(init, coords);
      el.dispatchEvent(new DragEvent(type, init));
    },
    { selector, text, type, coords },
  );
}

// Compute a clientX/Y inside a target task, in its top or bottom half.
async function _taskCoords(page, text, half) {
  return page.evaluate(
    ({ text, half }) => {
      const el = [...document.querySelectorAll("[phx-dragstart='drag_start']")].find((e) =>
        e.textContent.includes(text),
      );
      const r = el.getBoundingClientRect();
      return { clientX: Math.round(r.left + 4), clientY: Math.round(half === "top" ? r.top + 4 : r.bottom - 4) };
    },
    { text, half },
  );
}

async function _columnCoords(page, name) {
  return page.evaluate(
    (name) => {
      const el = [...document.querySelectorAll("[phx-dragover='drag_over_column']")].find((c) =>
        c.textContent.includes(name),
      );
      const r = el.getBoundingClientRect();
      return { clientX: Math.round(r.left + 4), clientY: Math.round(r.bottom - 4) };
    },
    name,
  );
}

/** Drag the task `srcText` so it lands immediately before `tgtText`. */
async function dragTaskBeforeTask(page, srcText, tgtText) {
  await _dispatchOn(page, TASK, srcText, "dragstart");
  await page.waitForTimeout(200);
  await _dispatchOn(page, TASK, tgtText, "dragover", await _taskCoords(page, tgtText, "top"));
  await page.waitForTimeout(200);
  await _dispatchOn(page, TASK, srcText, "dragend");
  await page.waitForTimeout(500);
}

/** Drag the task `srcText` to the end of column `columnName` (e.g. across columns). */
async function dragTaskToColumn(page, srcText, columnName) {
  await _dispatchOn(page, TASK, srcText, "dragstart");
  await page.waitForTimeout(200);
  await _dispatchOn(page, COLUMN, columnName, "dragover", await _columnCoords(page, columnName));
  await page.waitForTimeout(200);
  await _dispatchOn(page, TASK, srcText, "dragend");
  await page.waitForTimeout(500);
}

module.exports = {
  COLUMN,
  TASK,
  columns,
  tasks,
  columnByName,
  taskCard,
  waitForBoard,
  createBoard,
  openBoard,
  addColumn,
  submitColumn,
  addTask,
  removeColumn,
  removeTask,
  taskTextsIn,
  columnNames,
  dragTaskBeforeTask,
  dragTaskToColumn,
};
