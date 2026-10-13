(() => {
  let nextId = 0;
  const icon = path => `<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.7" aria-hidden="true"><path d="${path}"/></svg>`;
  const outward = icon('m7 9 5-5 5 5M7 15l5 5 5-5');
  const inward = icon('m7 4 5 5 5-5M7 20l5-5 5 5');
  function positionHighlight(content) {
    if (content.hidden) return;
    const code = content.closest('code');
    const bounds = content.getBoundingClientRect(), origin = code.getBoundingClientRect();
    const linePadding = (parseFloat(getComputedStyle(code).lineHeight) - content.getClientRects()[0].height) / 2;
    const highlight = content.nextElementSibling;
    highlight.style.left = `${bounds.left - origin.left - code.clientLeft + code.scrollLeft}px`;
    highlight.style.top = `${bounds.top - origin.top - code.clientTop + code.scrollTop - linePadding}px`;
    highlight.style.width = `${bounds.width}px`;
    highlight.style.height = `${bounds.height + linePadding * 2}px`;
  }
  window.addEventListener('resize', () => {
    document.querySelectorAll('.exdoc-fold.is-expanded > .exdoc-fold-content').forEach(positionHighlight);
  });
  function initialize() {
    document.querySelectorAll('pre[data-exdoc-folds]:not([data-folds-ready])').forEach(pre => {
      const code = pre.querySelector('code'), source = code.textContent;
      const indents = source.split('\n').filter(line => line.trim()).map(line => line.match(/^[ \t]*/)[0].length);
      const indentWidth = Math.min(...indents.filter(indent => indent > 0));
      const starts = [0];
      for (let i = 0; i < source.length; i++) if (source[i] === '\n') starts.push(i + 1);
      starts.push(source.length);
      const walker = document.createTreeWalker(code, NodeFilter.SHOW_TEXT), nodes = [];
      let node, offset = 0;
      while ((node = walker.nextNode())) {
        nodes.push({node, start: offset, end: offset + node.length});
        offset += node.length;
      }
      function point(offset) {
        const item = nodes.find(item => offset <= item.end);
        return [item.node, offset - item.start];
      }
      function slice(start, end) {
        const range = document.createRange();
        range.setStart(...point(start)); range.setEnd(...point(end));
        const span = document.createElement('span');
        let fragment = range.cloneContents();
        let ancestor = range.commonAncestorContainer;
        if (ancestor.nodeType === Node.TEXT_NODE) ancestor = ancestor.parentElement;
        while (ancestor !== code) {
          const shell = ancestor.cloneNode(false);
          shell.append(fragment);
          fragment = shell;
          ancestor = ancestor.parentElement;
        }
        span.append(fragment);
        return span;
      }
      const result = document.createDocumentFragment(), regions = [];
      let cursor = 0;
      pre.dataset.exdocFolds.split(';').forEach(entry => {
        const [from, to, encoded] = entry.split(':');
        const start = starts[Number(from)], end = starts[Number(to)];
        const heading = new TextDecoder().decode(Uint8Array.from(atob(encoded), c => c.charCodeAt(0)));
        const previewMatch = heading.match(/^`([^`]+)`\s*\|\s*(.*)$/);
        const label = previewMatch ? previewMatch[2] : heading;
        result.append(slice(cursor, start));
        const region = document.createElement('span'); region.className = 'exdoc-fold';
        const content = slice(start, end); content.className = 'exdoc-fold-content';
        content.id = `exdoc-fold-${++nextId}`;
        const highlight = document.createElement('span');
        highlight.className = 'exdoc-fold-highlight';
        highlight.setAttribute('aria-hidden', 'true');
        const button = document.createElement('button');
        button.type = 'button'; button.className = 'exdoc-fold-control';
        button.innerHTML = icon('m9 6 6 6-6 6');
        button.dataset.label = `${label || 'Hidden code'} (${Number(to) - Number(from)} lines)`;
        if (previewMatch) {
          const following = source.slice(end).match(/^(?:[ \t]*\n)*([ \t]*)/)[1];
          const previewIndent = Math.max(0, following.length - indentWidth);
          const preview = document.createElement('span');
          preview.className = 'exdoc-fold-preview';
          preview.dataset.preview = following.slice(0, previewIndent) + previewMatch[1];
          button.append(preview);
        }
        button.setAttribute('aria-controls', content.id);
        let glow;
        function setExpanded(expanded) {
          const newlyRevealed = expanded && content.hidden;
          glow?.cancel();
          region.classList.toggle('is-expanded', expanded); content.hidden = !expanded;
          button.setAttribute('aria-expanded', String(expanded));
          button.setAttribute('aria-label', `${expanded ? 'Collapse' : 'Expand'} ${label || 'hidden code'} (${Number(to) - Number(from)} lines)${previewMatch ? `: ${previewMatch[1]}` : ''}`);
          button.title = expanded ? `Collapse ${label || 'marked region'}` : button.dataset.label;
          if (expanded) positionHighlight(content);
          if (newlyRevealed && !window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
            glow = highlight.animate([
              { opacity: 1, backgroundColor: 'rgba(255, 210, 40, 0.32)' },
              { opacity: 0, backgroundColor: 'transparent' }
            ], { duration: 700, easing: 'ease-out' });
          }
        }
        button.addEventListener('click', () => { setExpanded(content.hidden); updateAll(); });
        button.addEventListener('pointerenter', () => positionHighlight(content));
        button.addEventListener('focus', () => positionHighlight(content));
        region.append(button, content, highlight); result.append(region);
        regions.push({content, setExpanded}); setExpanded(false); cursor = end;
      });
      result.append(slice(cursor, source.length)); code.replaceChildren(result);
      const all = document.createElement('button'); all.type = 'button';
      all.className = 'exdoc-fold-all';
      all.setAttribute('aria-controls', regions.map(({content}) => content.id).join(' '));
      function updateAll() {
        const expanded = regions.every(({content}) => !content.hidden);
        const label = expanded ? 'Collapse all marked regions' : 'Expand all hidden code';
        all.innerHTML = expanded ? inward : outward;
        all.setAttribute('aria-label', label); all.setAttribute('aria-expanded', String(expanded));
        all.title = label;
      }
      all.addEventListener('click', () => {
        const expand = regions.some(({content}) => content.hidden);
        regions.forEach(({setExpanded}) => setExpanded(expand)); updateAll();
      });
      pre.append(all); updateAll();
      pre.dataset.foldsReady = '';
    });
  }
  window.addEventListener('exdoc:loaded', initialize);
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initialize);
  else initialize();
})();
