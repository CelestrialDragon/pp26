// A rope drawn as a tree, and three animations on it.
//   RopeViz.mount(el, { rope, mode, ... })  returns { finish() } so the print shows the final frame.
// A rope is written as nested arrays: a string is a leaf, [l, r] is Cat (l, r).
//   mode 'static'  the tree; opt.mark = { new: [paths], shared: [paths] } tints nodes (a path is 'lr…', '' the root)
//   mode 'iter'    a traversal, leaf by leaf, with the counter of calls; opt.target is the element looked for;
//                  opt.stop = true leaves the traversal at the target (fold_until)
//   mode 'fold'    fold leaf cat: every Leaf becomes leaf x, every Cat becomes cat a b, left subtree first;
//                  opt.leaf, opt.cat are functions, opt.leafName, opt.catName how they are written, opt.show prints a value
//   mode 'same'    two ropes (opt.rope, opt.other), one sequence on each, advanced in step until they differ
//   mode 'versions' one rope, then a second version in which the leaf at opt.path is replaced by opt.leaf:
//                  the nodes on the path from the root to that leaf are copied, every other subtree is shared
(function () {
  'use strict';
  const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;');
  const label = x => x === '\n' ? '\\n' : x === ' ' ? '␣' : String(x);

  // layout: leaves at consecutive x, an inner node over the middle of its children, y by depth
  function lay(rope) {
    const nodes = []; let x = 0;
    (function go(t, d, parent, path) {
      const n = { d, parent, path, id: nodes.length }; nodes.push(n);
      if (Array.isArray(t)) { n.l = go(t[0], d + 1, n, path + 'l'); n.r = go(t[1], d + 1, n, path + 'r'); n.x = (n.l.x + n.r.x) / 2; }
      else { n.leaf = t; n.x = x++; }
      return n;
    })(rope, 0, null, '');
    return { nodes, leaves: nodes.filter(n => 'leaf' in n), cols: x, rows: Math.max(...nodes.map(n => n.d)) + 1 };
  }

  // one tree as SVG markup inside the box (x0, y0, w, h); ids are prefixed so two trees can share an svg
  function tree(L, box, prefix, opt) {
    const lw = opt.leafW || 64, gapx = L.cols > 1 ? (box.w - lw) / (L.cols - 1) : 0, gapy = L.rows > 1 ? (box.h - lw) / (L.rows - 1) : 0;
    const px = n => box.x + lw / 2 + n.x * gapx, py = n => box.y + lw / 2 + n.d * gapy;
    let s = '';
    L.nodes.forEach(n => { if (n.parent) s += '<line class="rv-e" data-id="' + prefix + 'e' + n.id + '" x1="' + px(n.parent) + '" y1="' + py(n.parent) + '" x2="' + px(n) + '" y2="' + py(n) + '"/>'; });
    L.nodes.forEach(n => {
      const X = px(n), Y = py(n);
      if ('leaf' in n) {
        const w = Math.max(lw, 30 + 22 * label(n.leaf).length);
        s += '<g class="rv-n rv-leaf" data-id="' + prefix + n.id + '"><rect x="' + (X - w / 2) + '" y="' + (Y - lw / 2) + '" width="' + w + '" height="' + lw + '" rx="8"/><text x="' + X + '" y="' + (Y + 12) + '">' + esc(label(n.leaf)) + '</text><text class="rv-val" x="' + X + '" y="' + (Y + lw / 2 + 34) + '"></text></g>';
      } else {
        s += '<g class="rv-n rv-cat" data-id="' + prefix + n.id + '"><circle cx="' + X + '" cy="' + Y + '" r="' + (opt.catR || 30) + '"/><text x="' + X + '" y="' + (Y + 9) + '">' + esc(opt.catLabel || 'Cat') + '</text><text class="rv-val" x="' + (X + (opt.catR || 30) + 12) + '" y="' + (Y - 14) + '"></text></g>';
      }
    });
    return s;
  }

  const controls = '<div class="an-ctl rv-ctl"><div><button class="run rv-step">Step</button> <button class="run rv-run">Run</button> <button class="run rv-reset">Reset</button></div><div class="rv-count"></div></div><div class="an-what rv-what"></div>';

  function stepper(el, total, render, ms) {
    let k = 0, timer = null;
    const q = s => el.querySelector(s), stop = () => { if (timer) { clearInterval(timer); timer = null; } };
    const draw = () => render(k);
    q('.rv-step').onclick = () => { stop(); if (k < total()) k++; draw(); };
    q('.rv-run').onclick = () => { stop(); if (k >= total()) k = 0; draw(); timer = setInterval(() => { if (k >= total()) return stop(); k++; draw(); }, ms || 700); };
    q('.rv-reset').onclick = () => { stop(); k = 0; draw(); };
    draw();
    return { finish() { stop(); k = total(); draw(); } };
  }

  function mount(el, opt) {
    if (!el) return { finish() {} };
    const o = Object.assign({ mode: 'static', w: 760, h: 330 }, opt);
    const L = lay(o.rope), W = o.w, H = o.h, pad = 16;
    el.classList.add('anim', 'ropeviz', 'rv-' + o.mode);
    const svgOpen = (w, h) => '<svg viewBox="0 0 ' + w + ' ' + h + '" class="rv-svg">';
    const q = s => el.querySelector(s);
    const g = (id) => el.querySelector('[data-id="' + id + '"]');
    const cls = (id, c, yes) => { const e = g(id); if (e) e.classList.toggle(c, !!yes); };

    if (o.mode === 'static') {
      el.innerHTML = svgOpen(W, H) + tree(L, { x: pad, y: pad, w: W - 2 * pad, h: H - 2 * pad - (o.caption ? 40 : 0) }, 'a', o) + '</svg>';
      const mark = o.mark || {};
      L.nodes.forEach(n => { if ((mark.new || []).includes(n.path)) cls('a' + n.id, 'new', true);
        if ((mark.shared || []).some(p => n.path.startsWith(p))) { cls('a' + n.id, 'shared', true); cls('ae' + n.id, 'shared', n.path !== '' && (mark.shared || []).some(p => n.path.length > p.length && n.path.startsWith(p))); }
        if ((mark.out || []).some(p => n.path.startsWith(p))) { cls('a' + n.id, 'out', true); cls('ae' + n.id, 'out', true); } });
      return { finish() {} };
    }

    if (o.mode === 'iter') {
      el.innerHTML = svgOpen(W, H) + tree(L, { x: pad, y: pad, w: W - 2 * pad, h: H - 2 * pad }, 'a', o) + '</svg>' + controls;
      const n = L.leaves.length, at = L.leaves.findIndex(l => l.leaf === o.target);
      const last = o.stop && at >= 0 ? at + 1 : n;
      return stepper(el, () => last, k => {
        L.leaves.forEach((l, i) => { cls('a' + l.id, 'done', i < k - 1); cls('a' + l.id, 'cur', i === k - 1); cls('a' + l.id, 'hit', i === at && k > at); cls('a' + l.id, 'skipped', o.stop && k === last && i >= last); });
        const found = at >= 0 && k > at;
        q('.rv-count').innerHTML = '<code>visited = ' + k + '</code> &nbsp; <code>found = ' + found + '</code>';
        q('.rv-what').textContent = k === 0 ? 'nothing visited yet'
          : k - 1 === at ? (o.stop ? 'leaf ' + k + ' is the newline; the traversal stops' : 'leaf ' + k + ' is the newline; the traversal goes on')
          : k === last ? 'the traversal is over: ' + k + ' leaves visited'
          : k - 1 > at && at >= 0 ? 'leaf ' + k + ': called, although the answer is known'
          : 'leaf ' + k + ': not a newline';
      }, o.ms);
    }

    if (o.mode === 'fold') {
      const showV = o.show || (v => JSON.stringify(v));
      el.innerHTML = svgOpen(W, H) + tree(L, { x: pad, y: pad + 6, w: W - 2 * pad - 90, h: H - 2 * pad - 50 }, 'a', o) + '</svg>' + controls;
      // post-order, left subtree first: the order in which the values become known
      const order = []; (function po(n) { if (n.l) { po(n.l); po(n.r); n.v = o.cat(n.l.v, n.r.v); } else n.v = o.leaf(n.leaf); order.push(n); })(L.nodes[0]);
      return stepper(el, () => order.length, k => {
        order.forEach((n, i) => { const known = i < k, e = g('a' + n.id); e.classList.toggle('known', known); e.classList.toggle('cur', i === k - 1); e.querySelector('.rv-val').textContent = known ? showV(n.v) : ''; });
        const n = k ? order[k - 1] : null;
        q('.rv-count').innerHTML = k === order.length ? '<code>result = ' + esc(showV(L.nodes[0].v)) + '</code>' : '';
        q('.rv-what').textContent = !n ? 'the rope: nothing computed yet'
          : 'leaf' in n ? 'Leaf ' + label(n.leaf) + ' becomes ' + o.leafName + ' ' + label(n.leaf) + ' = ' + showV(n.v)
          : 'Cat becomes ' + o.catName + ' ' + showV(n.l.v) + ' ' + showV(n.r.v) + ' = ' + showV(n.v);
      }, o.ms);
    }

    if (o.mode === 'same') {
      const L2 = lay(o.other), half = (W - 60) / 2, TH = H - 130;
      // the two fringes, written under the trees, element under element
      const fringe = (LL, x0, prefix) => LL.leaves.map((l, i) => '<g class="rv-f" data-id="' + prefix + 'f' + i + '"><rect x="' + (x0 + i * 58) + '" y="' + (TH + 50) + '" width="50" height="50" rx="6"/><text x="' + (x0 + i * 58 + 25) + '" y="' + (TH + 85) + '">' + esc(label(l.leaf)) + '</text></g>').join('');
      el.innerHTML = svgOpen(W, H) + tree(L, { x: pad, y: pad, w: half - 2 * pad, h: TH - pad }, 'a', o) + tree(L2, { x: half + 60 + pad, y: pad, w: half - 2 * pad, h: TH - pad }, 'b', o)
        + fringe(L, pad + (half - 2 * pad - L.leaves.length * 58) / 2, 'a') + fringe(L2, half + 60 + pad + (half - 2 * pad - L2.leaves.length * 58) / 2, 'b') + '</svg>' + controls;
      const n = Math.min(L.leaves.length, L2.leaves.length);
      let diff = -1; for (let i = 0; i < n; i++) if (L.leaves[i].leaf !== L2.leaves[i].leaf) { diff = i; break; }
      const sameLen = L.leaves.length === L2.leaves.length;
      const last = diff >= 0 ? diff + 1 : n + 1;          // one more step to see that both sequences are done
      return stepper(el, () => last, k => {
        [['a', L], ['b', L2]].forEach(([p, LL]) => LL.leaves.forEach((l, i) => {
          const cur = i === k - 1, done = i < k - 1, bad = cur && i === diff;
          cls(p + l.id, 'cur', cur); cls(p + l.id, 'done', done); cls(p + l.id, 'bad', bad);
          cls(p + 'f' + i, 'cur', cur); cls(p + 'f' + i, 'done', done); cls(p + 'f' + i, 'bad', bad);
        }));
        const asked = Math.min(k, n) * 2;
        q('.rv-count').innerHTML = '<code>leaves touched = ' + asked + ' of ' + (L.leaves.length + L2.leaves.length) + '</code>';
        q('.rv-what').textContent = k === 0 ? 'one sequence on each rope; nothing asked yet'
          : k - 1 === diff ? 'element ' + k + ' differs: the answer is false'
          : k > n ? (sameLen ? 'both sequences are done: the answer is true' : 'one sequence is done first: the answer is false')
          : 'element ' + k + ': equal, both sequences advance';
      }, o.ms);
    }
    if (o.mode === 'versions') {
      // the old version is laid out as a tree; each copy is drawn beside the node it copies, up and to the right
      const lw = o.leafW || 64, R = o.catR || 30, SX = o.shiftX || 1, SY = o.shiftY || 0.5, TOP = 44;
      const gapx = (W - 2 * pad - lw - 30) / (L.cols - 1 + SX), gapy = (H - TOP - pad - lw) / (L.rows - 1 + SY);
      const box = { x: pad, y: TOP + SY * gapy, w: lw + gapx * (L.cols - 1), h: lw + gapy * (L.rows - 1) };
      const px = n => box.x + lw / 2 + n.x * gapx, py = n => box.y + lw / 2 + n.d * gapy;
      const cx = n => px(n) + SX * gapx, cy = n => py(n) - SY * gapy;
      const path = []; { let n = L.nodes[0]; path.push(n); for (const c of o.path) { n = c === 'l' ? n.l : n.r; path.push(n); } }
      const leafBox = (X, Y, text, cls, id) => { const w = Math.max(lw, 30 + 22 * label(text).length); return '<g class="rv-n rv-leaf ' + cls + '" data-id="' + id + '"><rect x="' + (X - w / 2) + '" y="' + (Y - lw / 2) + '" width="' + w + '" height="' + lw + '" rx="8"/><text x="' + X + '" y="' + (Y + 12) + '">' + esc(label(text)) + '</text></g>'; };
      let s = '';
      // the copies' edges first, so that they pass under the nodes
      path.forEach((n, i) => { if (!i) return; const up = path[i - 1], other = up.l === n ? up.r : up.l;
        s += '<line class="rv-e rv-new" data-id="ne' + i + '" x1="' + cx(up) + '" y1="' + cy(up) + '" x2="' + cx(n) + '" y2="' + cy(n) + '"/>';
        s += '<line class="rv-e rv-new rv-to-shared" data-id="ns' + i + '" x1="' + cx(up) + '" y1="' + cy(up) + '" x2="' + px(other) + '" y2="' + py(other) + '"/>'; });
      s += tree(L, box, 'a', o).replace(/<text class="rv-val"[^>]*><\/text>/g, '');
      path.forEach((n, i) => { s += i === path.length - 1 ? leafBox(cx(n), cy(n), o.leaf, 'new', 'n' + i)
        : '<g class="rv-n rv-cat new" data-id="n' + i + '"><circle cx="' + cx(n) + '" cy="' + cy(n) + '" r="' + R + '"/><text x="' + cx(n) + '" y="' + (cy(n) + 9) + '">Cat</text></g>'; });
      s += '<text class="rv-root" data-id="r1" x="' + px(path[0]) + '" y="' + (py(path[0]) - R - 14) + '">version 1</text>';
      s += '<text class="rv-root new" data-id="r2" x="' + cx(path[0]) + '" y="' + (cy(path[0]) - R - 14) + '">version 2</text>';
      el.innerHTML = svgOpen(W, H) + s + '</svg>' + controls;
      const m = path.length;                      // steps: the new leaf, then one copy per node up to the root, then the sharing
      const shared = L.nodes.filter(n => !path.includes(n) && path.includes(n.parent));
      const under = (n, top) => { for (let x = n; x; x = x.parent) if (x === top) return true; return false; };
      return stepper(el, () => m + 1, k => {
        path.forEach((n, i) => { const shown = k >= m - i; g('n' + i).style.display = shown ? '' : 'none';
          if (i) { g('ne' + i).style.display = k >= m - i + 1 ? '' : 'none'; g('ns' + i).style.display = k >= m - i + 1 ? '' : 'none'; } });
        g('r2').style.display = k >= m ? '' : 'none';
        L.nodes.forEach(n => cls('a' + n.id, 'shared', k > m && shared.some(t => under(n, t))));
        q('.rv-count').innerHTML = k > m ? '<code>copied = ' + m + ' &nbsp; shared = ' + L.nodes.filter(n => shared.some(t => under(n, t))).length + '</code>' : '';
        q('.rv-what').textContent = k === 0 ? 'version 1'
          : k === 1 ? 'the new leaf'
          : k < m ? 'its parent is copied, with one new child'
          : k === m ? 'the root is copied: version 2 has its own root'
          : 'the rest is shared; version 1 is unchanged';
      }, o.ms);
    }
    return { finish() {} };
  }

  window.RopeViz = { mount };
})();
