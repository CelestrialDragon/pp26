// Provided; not to be edited. A rope drawn as a tree.
//   RopeView.draw(el, { tree, above, first, leaves })
// A node of the tree is { leaf, lit, caret, shared } or { l, r, shared }:
// lit is a leaf that the last command visited, caret the leaf under the caret,
// shared a node that the version compared with has too.
(function () {
  'use strict';
  const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;');
  const label = c => c === '\n' ? '\\n' : c === ' ' ? '␣' : c === '\t' ? '\\t' : c;

  // leaves at consecutive x, an inner node over the middle of its children, y by depth
  function lay(tree) {
    const nodes = []; let x = 0;
    (function go(t, d, parent) {
      const n = { d, parent, t }; nodes.push(n);
      if ('leaf' in t) n.x = x++;
      else { n.l = go(t.l, d + 1, n); n.r = go(t.r, d + 1, n); n.x = (n.l.x + n.r.x) / 2; }
      return n;
    })(tree, 0, null);
    return { nodes, cols: x, rows: Math.max(...nodes.map(n => n.d)) + 1 };
  }

  function draw(el, data) {
    if (!data) { el.innerHTML = ''; return; }
    const L = lay(data.tree), gx = 34, gy = 46, pad = 22;
    const W = pad * 2 + (L.cols - 1) * gx + 28, H = pad * 2 + (L.rows - 1) * gy + 28;
    const px = n => pad + 14 + n.x * gx, py = n => pad + 14 + n.d * gy;
    let s = '<svg viewBox="0 0 ' + W + ' ' + H + '" class="rv" style="max-width:' + Math.max(W, 120) + 'px" role="img" aria-label="the rope around the caret">';
    L.nodes.forEach(n => { if (n.parent) s += '<line class="rv-e' + (n.t.shared ? ' shared' : '') + '" x1="' + px(n.parent) + '" y1="' + py(n.parent) + '" x2="' + px(n) + '" y2="' + py(n) + '"/>'; });
    L.nodes.forEach(n => {
      const X = px(n), Y = py(n), t = n.t;
      const cls = (t.shared ? ' shared' : '') + (t.lit ? ' lit' : '') + (t.caret ? ' caret' : '');
      if ('leaf' in t) s += '<g class="rv-n rv-leaf' + cls + '"><rect x="' + (X - 13) + '" y="' + (Y - 14) + '" width="26" height="28" rx="4"/><text x="' + X + '" y="' + (Y + 5) + '">' + esc(label(t.leaf)) + '</text></g>';
      else s += '<g class="rv-n rv-cat' + cls + '"><circle cx="' + X + '" cy="' + Y + '" r="7"/></g>';
    });
    el.innerHTML = s + '</svg>';
  }

  window.RopeView = { draw };
})();
