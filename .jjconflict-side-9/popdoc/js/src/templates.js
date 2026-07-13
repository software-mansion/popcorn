const tpl = (html) => {
  const t = document.createElement("template");
  t.innerHTML = html.trim();
  return t;
};

export function instantiate(template) {
  return template.content.firstElementChild.cloneNode(true);
}

export const TPL_BLOCK = tpl(`
  <div class="popdoc-block">
    <div class="popdoc-header">
      <button class="popdoc-run" type="button" disabled>Run</button>
      <span class="popdoc-status" hidden></span>
    </div>
    <div class="popdoc-output" hidden></div>
  </div>
`);

export const TPL_STATUS = tpl(`
  <span>
    <span class="popdoc-dot"></span>
    <span></span>
  </span>
`);

export const TPL_SKELETON = tpl(`
  <div class="popdoc-result"><span class="popdoc-empty">(empty)</span></div>
`);

export const TPL_TOGGLE = tpl(`
  <button type="button" class="popdoc-toggle">
    <span class="popdoc-chev">▸</span>
    <span>show all results</span>
  </button>
`);

export const TPL_TOP = tpl(`
  <div class="popdoc-top">
    <div class="popdoc-result"></div>
  </div>
`);

export const TPL_STDIO = tpl(`
  <div class="popdoc-stdio">
    <span class="popdoc-label"></span>
    <span></span>
  </div>
`);

export const TPL_ROW = tpl(`
  <div class="popdoc-row">
    <span class="popdoc-cell-snippet"></span>
    <span class="popdoc-cell-result"></span>
  </div>
`);

export const TPL_ERROR_TOP = tpl(`
  <div class="popdoc-top-error">
    <span class="popdoc-cell-snippet"></span>
    <span class="popdoc-cell-result popdoc-result-err"></span>
  </div>
`);

export const TPL_STACKTRACE_TOGGLE = tpl(`
  <button type="button" class="popdoc-toggle popdoc-stacktrace-toggle">
    <span class="popdoc-chev">▸</span>
    <span>show stacktrace</span>
  </button>
`);

export const TPL_STACKTRACE = tpl(`
  <pre class="popdoc-stacktrace" hidden></pre>
`);
