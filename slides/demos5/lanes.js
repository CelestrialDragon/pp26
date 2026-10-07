// Two lanes, one per party, and the control passing from one to the other.
//   Lanes.mount(el, { left, right, events, w, h })  returns { finish() } for the print.
// Time runs downwards. An event is { to: 'right' | 'left', label, what, gap, end }:
//   to     the lane that receives the control
//   label  written on the arrow
//   what   the status line shown at this step
//   gap    true: a break in the lanes is drawn before this event (steps left out)
//   end    true: the lane that gives the control away ends here (the function has returned)
// The lane that has the control is drawn thick; a lane that waits, suspended, is dashed.
(function () {
  'use strict';
  const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;');
  function mount(el, opt) {
    if (!el) return { finish() {} };
    const o = Object.assign({ w: 700, h: 520, left: 'the consumer', right: 'the generator', events: [] }, opt);
    const XL = 0.24 * o.w, XR = 0.76 * o.w, TOP = 96, n = o.events.length;
    const gaps = o.events.filter(e => e.gap).length;
    const ROW = (o.h - TOP - 30 - gaps * 34) / n;
    const ys = []; { let y = TOP; o.events.forEach(e => { if (e.gap) y += 34; y += ROW; ys.push(y - ROW / 2 + 8); }); }
    el.classList.add('anim', 'lanes');
    let s = '<svg viewBox="0 0 ' + o.w + ' ' + o.h + '" class="ln-svg">';
    s += '<defs><marker id="ln-ah" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto"><path d="M0,0 L10,5 L0,10 z" class="ln-head"/></marker></defs>';
    [[XL, o.left], [XR, o.right]].forEach(([x, name]) => { s += '<rect class="ln-who" x="' + (x - 140) + '" y="8" width="280" height="56" rx="8"/><text class="ln-name" x="' + x + '" y="46">' + esc(name) + '</text>'; });
    s += '<g class="ln-body"></g></svg>';
    el.innerHTML = s + '<div class="an-ctl ln-ctl"><div><button class="run ln-step">Step</button> <button class="run ln-run">Run</button> <button class="run ln-reset">Reset</button></div></div><div class="an-what ln-what"></div>';
    const q = x => el.querySelector(x);
    let k = 0, timer = null;
    const stop = () => { if (timer) { clearInterval(timer); timer = null; } };
    function render() {
      // walk through the events shown so far, drawing for each lane what it did in between
      let b = '', ctl = 'left', y0 = 64, rightState = 'none';        // none: not started; wait: suspended; over: returned
      const seg = (x, a, c, cls) => { if (c > a) b += '<line class="ln-seg ' + cls + '" x1="' + x + '" y1="' + a + '" x2="' + x + '" y2="' + c + '"/>'; };
      const lanes = (a, c) => {
        seg(XL, a, c, ctl === 'left' ? 'has' : 'idle');
        if (ctl === 'right') seg(XR, a, c, 'has'); else if (rightState === 'wait') seg(XR, a, c, 'wait');
      };
      for (let i = 0; i < k; i++) {
        const e = o.events[i], y = ys[i];
        if (e.gap) { lanes(y0, y - ROW / 2 - 34); b += '<text class="ln-gap" x="' + (o.w / 2) + '" y="' + (y - ROW / 2 - 20) + '">…</text>'; y0 = y - ROW / 2; }
        lanes(y0, y);
        const from = e.to === 'right' ? XL : XR, to = e.to === 'right' ? XR : XL, d = e.to === 'right' ? 1 : -1;
        b += '<line class="ln-arrow' + (i === k - 1 ? ' cur' : '') + '" x1="' + (from + 10 * d) + '" y1="' + y + '" x2="' + (to - 12 * d) + '" y2="' + y + '" marker-end="url(#ln-ah)"/>';
        b += '<text class="ln-label' + (i === k - 1 ? ' cur' : '') + '" x="' + (o.w / 2) + '" y="' + (y - 12) + '">' + esc(e.label) + '</text>';
        if (e.to === 'left') rightState = e.end ? 'over' : 'wait';
        ctl = e.to; y0 = y;
      }
      lanes(y0, y0 + (k < n ? ROW * 0.45 : 24));
      if (rightState === 'over') b += '<line class="ln-stop" x1="' + (XR - 26) + '" y1="' + (y0 + 2) + '" x2="' + (XR + 26) + '" y2="' + (y0 + 2) + '"/>';
      q('.ln-body').innerHTML = b;
      q('.ln-what').textContent = k === 0 ? (o.start || 'nothing has run yet') : o.events[k - 1].what;
    }
    q('.ln-step').onclick = () => { stop(); if (k < n) k++; render(); };
    q('.ln-run').onclick = () => { stop(); if (k >= n) k = 0; render(); timer = setInterval(() => { if (k >= n) return stop(); k++; render(); }, 1100); };
    q('.ln-reset').onclick = () => { stop(); k = 0; render(); };
    render();
    return { finish() { stop(); k = n; render(); } };
  }
  window.Lanes = { mount };
})();
