// Block 2's animations: folds that stop or do not, thunks, sequences, sharing.
//   SeqViz.mount(el, { mode, ... })  returns { finish() } for the print.
// Every mode computes a list of frames; a frame says where each thing is, how it looks, and what
// the status line reads. Things keep their identity from frame to frame: a thing that changes
// place moves there, and a thing that appears comes out of the place given as `from`.
//   mode 'fold'      a fold that looks for the newline in h i \n y o, drawn as session 4 drew
//                    folds: the list as a tree of (:) nodes, the fold as a tree of f nodes
//                    variant 'left' | 'right' | 'thunk'
//   mode 'cells'     a list and the sequence nats 0, as boxes and pointers; opt.n calls
//   mode 'pipeline'  take n (map square (filter even (nats 0))): requests go left, elements right
//   mode 'twice'     a thunk and a lazy value, each asked twice
//   mode 'fibs'      the list of Fibonacci numbers read at two places; opt.n cells
//   mode 'lazylist'  Haskell's [0 ..] read by take n: a cell is built when it is read
// Kinds of things: node (a circle), leaf (an element), seal (a computation not yet run),
// pair (a cell: a value and a pointer), card, chip, edge, label.
(function () {
  'use strict';
  const NS = 'http://www.w3.org/2000/svg';
  const clone = o => JSON.parse(JSON.stringify(o));
  const show = c => c === '\n' ? '\\n' : c;
  let serial = 0;

  // ---- frames ---------------------------------------------------------------------------
  const MODES = {};

  MODES.fold = function (o) {
    const xs = ['h', 'i', '\n', 'y', 'o'], n = xs.length, p = c => c === '\n';
    const DX = 84, DY = 62, TY = 84, LX = 190, RX = o.variant === 'left' ? 850 : 830;
    const frames = [];
    // the list, on the left: (:) nodes down a diagonal, the elements at their left, [] at the end
    const list = used => {
      const it = {};
      for (let i = 0; i < n; i++) {
        const x = LX + i * DX, y = TY + i * DY, u = i < used ? ' used' : '';
        it['le' + i] = { kind: 'edge', x1: x, y1: y, x2: x - DX + 8, y2: y + DY, cls: u };
        it['lc' + i] = { kind: 'edge', x1: x, y1: y, x2: x + DX, y2: y + DY, cls: u };
        it['c' + i] = { kind: 'node', x, y, text: ':', cls: 'cons' + u };
        it['x' + i] = { kind: 'leaf', x: x - DX + 8, y: y + DY, text: show(xs[i]), cls: u };
      }
      it.nil = { kind: 'leaf', x: LX + n * DX, y: TY + n * DY, text: '[]', cls: 'bare' + (used > n ? ' used' : '') };
      it.cap = { kind: 'label', x: 40, y: 30, text: 'the list', cls: 'cap' };
      it.cap2 = { kind: 'label', x: 700, y: 30, text: o.variant === 'left' ? 'fold_left f false text' : 'fold_right f text false', cls: 'cap mono' };
      return it;
    };
    const push = (it, what, called) => {
      Object.keys(it).forEach(id => { if (it[id].kind === 'edge') { it[id].t1 = 34; it[id].t2 = 36; } });
      frames.push({ items: it, what, counters: [['f called', called, called === 1 ? 'time' : 'times']] });
    };
    const badge = v => ({ badge: String(v), bcls: v ? 'yes' : 'no' });

    if (o.variant === 'left') {
      // the mirrored tree: every f takes what is below it and one element; it grows upwards
      const BY = TY + (n - 1) * DY;
      const P = i => ({ x: RX + i * DX, y: BY - i * DY });
      const grow = (k, cur) => {
        const it = list(k);
        it.z = { kind: 'leaf', x: RX - DX + 8, y: BY + DY, text: 'false', cls: 'seed' };
        let found = false;
        for (let i = 0; i < k; i++) {
          const { x, y } = P(i);
          found = found || p(xs[i]);
          it['fa' + i] = { kind: 'edge', x1: x, y1: y, x2: x - DX + 8, y2: y + DY, cls: '' };
          it['fe' + i] = { kind: 'edge', x1: x, y1: y, x2: x + DX - 8, y2: y + DY, cls: '' };
          it['f' + i] = Object.assign({ kind: 'node', x, y, text: 'f', cls: 'fn' + (i === cur ? ' cur' : ' done'), bside: 'left' }, badge(found));
          it['y' + i] = { kind: 'leaf', x: x + DX - 8, y: y + DY, text: show(xs[i]), cls: '' };
        }
        if (k < n) {
          it.hold = { kind: 'label', x: LX + k * DX + 60, y: TY + k * DY - 6, text: 'the rest: the fold keeps it', cls: 'note start' };
        }
        return it;
      };
      push(grow(0, -1), 'the fold starts from false; f has not been called', 0);
      let found = false;
      for (let i = 0; i < n; i++) {
        const before = found; found = found || p(xs[i]);
        push(grow(i + 1, i), before ? "f is called on '" + show(xs[i]) + "': the answer below it is true already, and f has nothing to do"
          : "f is called on '" + show(xs[i]) + "' with false from below: it answers " + found, i + 1);
      }
      push(grow(n, -1), 'f was given one element at a time. The rest of the list never reached it: 5 calls', n);
    } else if (o.variant === 'right') {
      // the same shape as the list: (:) becomes f, [] becomes false; then the values rise
      const vals = []; { let r = false; for (let i = n - 1; i >= 0; i--) { r = p(xs[i]) || r; vals[i] = r; } }
      const tree = (made, z, risen) => {                   // made: f nodes written; risen: f nodes called, from the bottom
        const it = list(0);
        for (let i = 0; i < made; i++) {
          const x = RX + i * DX, y = TY + i * DY, called = i >= n - risen;
          it['fe' + i] = { kind: 'edge', x1: x, y1: y, x2: x - DX + 8, y2: y + DY, cls: '' };
          if (i < made - 1 || z) it['fr' + i] = { kind: 'edge', x1: x, y1: y, x2: x + DX, y2: y + DY, cls: '' };
          it['f' + i] = Object.assign({ kind: 'node', x, y, text: 'f', cls: 'fn ' + (called ? (i === n - risen ? 'cur' : 'done') : 'pending') }, called ? badge(vals[i]) : {});
          it['y' + i] = { kind: 'leaf', x: x - DX + 8, y: y + DY, text: show(xs[i]), cls: '' };
          it['c' + i].cls = 'cons used'; it['x' + i].cls = 'used'; it['le' + i].cls = 'used'; it['lc' + i].cls = 'used';
        }
        if (z) { it.z = { kind: 'leaf', x: RX + n * DX, y: TY + n * DY, text: 'false', cls: 'seed' }; it.nil.cls = 'bare used'; }
        return it;
      };
      push(tree(0, false, 0), 'the list, as a tree of (:) nodes; f has not been called', 0);
      for (let i = 0; i < n; i++)
        push(tree(i + 1, false, 0), "to call f on '" + show(xs[i]) + "', OCaml must first compute its second argument: the fold of the rest", 0);
      push(tree(n, true, 0), 'the list is read to its end. Every f is waiting; none has been called', 0);
      for (let r = 1; r <= n; r++) {
        const i = n - r;
        push(tree(n, true, r), "f is called on '" + show(xs[i]) + "'. Its second argument is " + (i === n - 1 ? false : vals[i + 1]) + ', already computed: it answers ' + vals[i], r);
      }
    } else {
      // the rest is handed over sealed; f opens it, or does not
      const tree = (open, cur, answered) => {              // open: f nodes that exist; answered: nodes whose answer is known
        const it = list(0);
        for (let i = 0; i < open; i++) {
          const x = RX + i * DX, y = TY + i * DY, known = i >= open - answered;
          it['fe' + i] = { kind: 'edge', x1: x, y1: y, x2: x - DX + 8, y2: y + DY, cls: '' };
          it['fr' + i] = { kind: 'edge', x1: x, y1: y, x2: x + DX, y2: y + DY, cls: i === open - 1 ? 'dash' : '' };
          it['f' + i] = Object.assign({ kind: 'node', x, y, text: 'f', cls: 'fn ' + (i === cur ? 'cur' : 'done') }, known ? badge(true) : {});
          it['y' + i] = { kind: 'leaf', x: x - DX + 8, y: y + DY, text: show(xs[i]), cls: i === cur && p(xs[i]) && answered ? 'hit' : '' };
          it['c' + i].cls = 'cons used'; it['x' + i].cls = 'used'; it['le' + i].cls = 'used'; it['lc' + i].cls = 'used';
        }
        const sx = RX + open * DX + 40, sy = TY + open * DY + 8;
        it.seal = { kind: 'seal', x: sx, y: sy, w: 250, h: 70, text: 'the rest', sub: answered ? 'never run' : 'not yet run', cls: answered ? 'never' : '', from: { x: sx - DX, y: sy - DY } };
        return it;
      };
      let it = tree(1, -1, 0); it.f0.cls = 'fn pending';
      push(it, 'fold_right calls f on the first element, and hands over the rest sealed: a thunk', 0);
      for (let i = 0; i < 3; i++) {
        push(tree(i + 1, i, 0), "f is called on '" + show(xs[i]) + "'. The rest of the fold has not run", i + 1);
        if (i < 2) {
          it = tree(i + 1, i, 0);
          it.seal.cls = 'opening';
          it.call = { kind: 'chip', x: RX + i * DX + 150, y: TY + i * DY - 6, w: 130, h: 46, text: 'rest ()', cls: 'ask', from: { x: RX + i * DX, y: TY + i * DY } };
          push(it, "p '" + show(xs[i]) + "' is false, so f asks for the rest: rest ()", i + 1);
        }
      }
      push(tree(3, 2, 1), "p '\\n' is true: || does not compute its right side. f answers true and never opens the rest", 3);
      push(tree(3, -1, 3), 'true goes up to the first call. y and o were never read: 3 calls', 3);
    }
    return { frames, w: 1400, h: 436 };
  };

  MODES.cells = function (o) {
    const n = o.n || 3, frames = [], YL = 120, YS = 330, U = 250, X = k => 330 + k * U;
    const base = () => {
      const it = {
        cap1: { kind: 'label', x: 40, y: 40, text: 'a list: every cell is built before we read the first one', cls: 'cap' },
        cap2: { kind: 'label', x: 40, y: YS - 96, text: 'a sequence: a cell is built when we call the thunk', cls: 'cap' },
        n1: { kind: 'label', x: 40, y: YL + 11, text: '[0; 1; 2]', cls: 'mono start' },
        n2: { kind: 'label', x: 40, y: YS + 11, text: 'nats 0', cls: 'mono start' },
        Lin: { kind: 'edge', x1: 210, y1: YL, x2: X(0) - 66, y2: YL, cls: 'arrow ptr' },
        Sin: { kind: 'edge', x1: 210, y1: YS, x2: X(0) - 66, y2: YS, cls: 'arrow ptr' },
        Lnil: { kind: 'label', x: X(3) - 20, y: YL + 11, text: '[]', cls: 'mono' }
      };
      for (let i = 0; i < 3; i++) {
        it['L' + i] = { kind: 'pair', x: X(i), y: YL, text: String(i), cls: '' };
        it['La' + i] = { kind: 'edge', x1: X(i) + 40, y1: YL, x2: X(i + 1) - (i < 2 ? 66 : 50), y2: YL, cls: 'arrow ptr' };
      }
      return it;
    };
    const seal = (k, cls) => ({ kind: 'seal', x: X(k) + 6, y: YS, w: 140, h: 64, text: 'nats ' + k, sub: '', cls: cls || '' });
    const tag = k => ({ kind: 'chip', x: X(k) - 128, y: YS + 46, w: 150, h: 40, text: 'nats ' + k + ' ()', cls: 'tag' });
    const it = base(); it.s0 = seal(0);
    frames.push({ items: clone(it), what: 'nats 0 is a thunk: no number has been computed', counters: [['cells built', 0]] });
    for (let k = 0; k < n; k++) {
      it['s' + k] = seal(k, 'opening');
      frames.push({ items: clone(it), what: 'we call the thunk: nats ' + k + ' ()', counters: [['cells built', k]] });
      delete it['s' + k];
      it['g' + k] = Object.assign(tag(k), { from: { x: X(k), y: YS } });
      it['p' + k] = { kind: 'pair', x: X(k), y: YS, text: String(k), cls: 'new' };
      it['q' + k] = { kind: 'edge', x1: X(k) + 40, y1: YS, x2: X(k + 1) - 66, y2: YS, cls: 'arrow ptr' };
      it['s' + (k + 1)] = Object.assign(seal(k + 1), { from: { x: X(k), y: YS } });
      frames.push({ items: clone(it), what: 'it builds one cell: the number ' + k + ', and the thunk nats ' + (k + 1), counters: [['cells built', k + 1]] });
      it['p' + k].cls = ''; delete it['g' + k].from; delete it['s' + (k + 1)].from;
    }
    return { frames, w: 1400, h: 400 };
  };

  MODES.pipeline = function (o) {
    // drawn in the order of the code: take n (map square (filter even (nats 0)))
    const n = o.n || 5, frames = [], Y = 205, UP = Y - 84, DOWN = Y + 84, TRAY = Y + 196;
    const SX = [200, 530, 860, 1190];                            // take, map, filter, nats
    const KX = i => 300 + i * 100;
    let produced = 0, next = 0; const res = [];
    const base = cur => {
      const it = {}, names = ['take ' + (n - res.length), 'map square', 'filter even', 'nats ' + next];
      names.forEach((name, i) => {
        it['s' + i] = { kind: 'card', x: SX[i], y: Y, w: 250, h: 84, text: name, cls: i === cur ? 'cur' : '' };
        if (i) {
          it['u' + i] = { kind: 'edge', x1: SX[i - 1], y1: UP, x2: SX[i], y2: UP, t1: 70, t2: 70, cls: 'lane arrow ask' };
          it['d' + i] = { kind: 'edge', x1: SX[i], y1: DOWN, x2: SX[i - 1], y2: DOWN, t1: 70, t2: 70, cls: 'lane arrow val' };
        }
      });
      it.r0 = { kind: 'label', x: SX[0], y: 34, text: 'the consumer', cls: 'cap mid' };
      it.r3 = { kind: 'label', x: SX[3], y: 34, text: 'the producer', cls: 'cap mid' };
      it.l1 = { kind: 'label', x: 1395, y: UP + 10, text: 'calls', cls: 'cap ask end' };
      it.l2 = { kind: 'label', x: 1395, y: DOWN + 10, text: 'cells', cls: 'cap val end' };
      it.tray = { kind: 'label', x: 20, y: TRAY + 10, text: 'the list built', cls: 'cap' };
      res.forEach((v, i) => { it['k' + i] = { kind: 'chip', x: KX(i), y: TRAY, w: 84, h: 46, text: String(v), cls: 'val kept' }; });
      return it;
    };
    // the expression, rewritten at each step: the elements kept, then what is still to compute
    const expr = (kept, k, m, hot) => {
      const b = (on, t) => on ? '<b>' + t + '</b>' : t;
      return kept.map((v, i) => b(hot.kept && i === kept.length - 1, v + ' ::')).join(' ') + (kept.length ? ' ' : '')
        + b(hot.take, 'take ' + k) + ' (map square (filter even (' + b(hot.nats, 'nats ' + m) + ')))';
    };
    const push = (it, what, e) => frames.push({ items: it, what, expr: e, counters: [['cells built by nats', produced]] });
    push(base(-1), 'squares is defined: three sequences, one inside the other. Nothing has run', expr([], n, 0, {}));
    while (res.length < n) {
      let it = base(0);
      it.req = { kind: 'chip', x: SX[3], y: UP, w: 78, h: 46, text: '()', cls: 'ask', from: { x: SX[0], y: UP } };
      push(it, 'take ' + (n - res.length) + ' needs an element: it calls the sequence of map, which calls that of filter, which calls nats ' + next, expr(res, n - res.length, next, { take: 1 }));
      for (;;) {
        const v = next; produced++;
        next++;
        it = base(3); it.s3.text = 'nats ' + v;
        it['v' + v] = { kind: 'chip', x: SX[2], y: DOWN, w: 84, h: 46, text: String(v), cls: 'val', from: { x: SX[3], y: DOWN } };
        push(it, 'nats ' + v + ' () builds one cell: ' + v + ', and the thunk nats ' + (v + 1), expr(res, n - res.length, v, { nats: 1 }));
        if (v % 2) {
          it = base(2);
          it['v' + v] = { kind: 'chip', x: SX[2], y: DOWN + 60, w: 84, h: 46, text: String(v), cls: 'val dead' };
          it.req = { kind: 'chip', x: SX[3], y: UP, w: 78, h: 46, text: '()', cls: 'ask', from: { x: SX[2], y: UP } };
          push(it, 'even ' + v + ' is false: filter does not give ' + v + ' to map. It calls the rest itself: nats ' + next, expr(res, n - res.length, next, { nats: 1 }));
        } else {
          it = base(1);
          const i = res.length;
          it.s0.text = 'take ' + (n - i - 1);
          it['k' + i] = { kind: 'chip', x: KX(i), y: TRAY, w: 84, h: 46, text: String(v * v), cls: 'val kept new', from: { x: SX[1], y: DOWN } };
          push(it, 'even ' + v + ' is true: filter gives ' + v + ' to map, map gives square ' + v + ' = ' + v * v + ' to take, which goes on as take ' + (n - i - 1), expr(res.concat([v * v]), n - i - 1, next, { kept: 1, take: 1, nats: 1 }));
          res.push(v * v);
          break;
        }
      }
    }
    const it = base(0); it.s0.cls = 'done';
    push(it, 'take 0 is the empty list: take calls nothing more, and nats ' + next + ' is never called', '<b>[' + res.join('; ') + ']</b>');
    return { frames, w: 1400, h: 450 };
  };

  MODES.twice = function () {
    const frames = [], BX = 440, SA = 900, SB = 1130, Y1 = 150, Y2 = 330;
    const base = () => ({
      l1: { kind: 'label', x: 40, y: Y1 + 10, text: 'a thunk', cls: 'cap' },
      l2: { kind: 'label', x: 40, y: Y2 + 10, text: 'a lazy value', cls: 'cap' },
      t: { kind: 'seal', x: BX, y: Y1, w: 300, h: 80, text: 'fun () -> 6 * 7', sub: '', cls: '' },
      z: { kind: 'seal', x: BX, y: Y2, w: 300, h: 80, text: 'lazy (6 * 7)', sub: '', cls: '' },
      ha: { kind: 'label', x: SA, y: 56, text: 'a', cls: 'mono' },
      hb: { kind: 'label', x: SB, y: 56, text: 'b', cls: 'mono' },
      ra: { kind: 'edge', x1: SA - 70, y1: 78, x2: SB + 70, y2: 78, cls: 'rule' }
    });
    const val = (x, y, cls) => ({ kind: 'chip', x, y, w: 110, h: 60, text: '42', cls: 'val big ' + (cls || ''), from: { x: BX, y } });
    const it = base(); let tt = 0, lt = 0;
    const push = what => frames.push({ items: clone(it), what, counters: [['the body of the thunk has run', tt], ['of the lazy value', lt]] });
    push('both are made. Nothing is computed');
    it.t.cls = 'opening'; it.ta = val(SA, Y1, 'new'); tt = 1; push('a = t (): the body runs, and gives 42');
    it.t.cls = ''; it.ta.cls = 'val big';
    it.z = { kind: 'seal', x: BX, y: Y2, w: 300, h: 80, text: '42', sub: '', cls: 'kept cur' }; it.za = val(SA, Y2, 'new'); lt = 1;
    push('a = Lazy.force l: the body runs, and 42 is kept in its place');
    it.z.cls = 'kept'; it.za.cls = 'val big';
    it.t.cls = 'opening'; it.tb = val(SB, Y1, 'new'); tt = 2; push('b = t (): the body runs again');
    it.t.cls = ''; it.tb.cls = 'val big'; it.z.cls = 'kept cur'; it.zb = val(SB, Y2, 'new');
    push('b = Lazy.force l: 42 is read. Nothing runs');
    it.z.cls = 'kept'; it.zb.cls = 'val big';
    return { frames, w: 1400, h: 420 };
  };

  MODES.lazylist = function (o) {
    const n = o.n || 5, frames = [], Y = 170, U = 190, X = k => 300 + k * U;
    const it = {
      name: { kind: 'label', x: 40, y: Y + 10, text: 'the list', cls: 'cap' },
      into: { kind: 'edge', x1: 160, y1: Y, x2: X(0) - 70, y2: Y, cls: 'arrow ptr' },
      rest: { kind: 'seal', x: X(0) + 12, y: Y, w: 150, h: 64, text: 'from 0', sub: '', cls: '' }
    };
    const push = (what, k) => frames.push({ items: clone(it), what, counters: [['cells built', k], ['cells in the list', 'no end']] });
    push('take receives from 0, not yet run. No cell of the list is built', 0);
    for (let k = 0; k < n; k++) {
      it.who = { kind: 'label', x: X(k), y: Y - 100, text: 'take ' + n, cls: 'mono reader' };
      it.down = { kind: 'edge', x: X(k), y: Y, x1: 0, y1: -84, x2: 0, y2: -40, cls: 'wire arrow' };
      it['c' + k] = { kind: 'pair', x: X(k), y: Y, text: String(k), cls: 'new', from: { x: X(k) + 12, y: Y } };
      it['p' + k] = { kind: 'edge', x1: X(k) + 40, y1: Y, x2: X(k + 1) - 70, y2: Y, cls: 'arrow ptr' };
      it.rest = { kind: 'seal', x: X(k + 1) + 12, y: Y, w: 150, h: 64, text: 'from ' + (k + 1), sub: '', cls: '' };
      push('take reads one more element: Haskell builds the cell with ' + k + ', and nothing after it', k + 1);
      it['c' + k].cls = ''; delete it['c' + k].from;
    }
    delete it.who; delete it.down;
    it.rest.cls = 'never'; it.rest.sub = 'never built'; it.rest.h = 84; it.rest.w = 190; it.rest.x = X(n) + 32;
    push('take has its five elements, and the program ends. The rest of the list, from 5, is never built', n);
    return { frames, w: 1400, h: 250 };
  };

  // ---- continuations (session 6, block 1) ---------------------------------------------------------------------------
  //   mode 'sumk'   sum_k [3; 1; 4] k0: the continuations are built, then called (demos/sum_k.ml)
  //   mode 'iterk'  iter_k (fun x k -> Cons (x, k)) on the melody ((C D) (E F)): the traversal
  //                 stops at every note, and resumes when its rest is called (demos/leaves.ml)
  //   mode 'star'   the pattern a*ab on a a a b: the search keeps ways back, and takes one
  //                 (demos/star.ml)
  MODES.sumk = function () {
    const xs = [3, 1, 4], n = xs.length, frames = [];
    const KX = 420, KY = i => 400 - i * 96, LX = i => 1010 + i * 96, LY = 150, VX = 800;
    const body = i => i === 0 ? 'k0 = fun v -> v' : 'k' + i + ' = fun v -> k' + (i - 1) + ' (' + xs[i - 1] + ' + v)';
    const list = l => '[' + l.join('; ') + ']';
    const base = (built, read, cur, called) => {
      const it = {
        cap1: { kind: 'label', x: 40, y: 40, text: 'the continuations', cls: 'cap' },
        cap2: { kind: 'label', x: 900, y: 40, text: 'the list', cls: 'cap' }
      };
      for (let i = 0; i <= built; i++) {
        it['k' + i] = { kind: 'card', x: KX, y: KY(i), w: 560, h: 72, text: body(i), cls: i === cur ? 'cur' : i > called ? 'done' : '' };
        if (i) it['a' + i] = { kind: 'edge', x1: 180, y1: KY(i) + 36, x2: 180, y2: KY(i - 1) - 36, t1: 2, t2: 6, cls: 'arrow ptr' };
      }
      xs.forEach((x, i) => { it['x' + i] = { kind: 'leaf', x: LX(i), y: LY, text: String(x), cls: i < read ? 'used' : '' }; });
      it.nil = { kind: 'leaf', x: LX(n), y: LY, text: '[]', cls: 'bare' + (read > n ? ' used' : '') };
      return it;
    };
    const push = (it, what, expr, b, c) => frames.push({ items: it, what, expr, counters: [['continuations built', b], ['called', c]] });
    push(base(0, 0, -1, 9), 'k0 returns what it receives. Nothing has been added', 'sum_k [3; 1; 4] k0', 0, 0);
    for (let i = 1; i <= n; i++) {
      const it = base(i, i, -1, 9);
      it['k' + i].from = { x: KX, y: KY(i - 1) };
      push(it, 'the list begins with ' + xs[i - 1] + ': we build k' + i + ', which adds ' + xs[i - 1] + ' and calls k' + (i - 1) + '. Nothing has been added',
        'sum_k ' + list(xs.slice(i)) + ' <b>k' + i + '</b>', i, 0);
    }
    let v = 0;
    for (let i = n; i >= 0; i--) {
      const it = base(n, n + 1, i, i);
      it.v = { kind: 'chip', x: VX, y: KY(i), w: 150, h: 52, text: 'v = ' + v, cls: 'val new', from: { x: LX(n), y: LY } };
      const what = i === n ? 'the list is empty: sum_k calls the last continuation built, k' + n + ', with 0'
        : i ? 'k' + (i + 1) + ' has added ' + xs[i] + ': it calls k' + i + ' with ' + v
        : 'k1 has added 3: it calls k0 with 8, and k0 returns 8';
      push(it, what, '<b>k' + i + ' ' + v + '</b>', n, n - i + 1);
      if (i) v += xs[i - 1];
    }
    const it = base(n, n + 1, -1, 0);
    it.v = { kind: 'chip', x: VX, y: KY(0), w: 150, h: 52, text: '8', cls: 'val' };
    push(it, 'every continuation was built once and called once, from the last one built to the first', '<b>8</b>', n, n + 1);
    return { frames, w: 1400, h: 460 };
  };

  MODES.iterk = function () {
    const frames = [], notes = ['C', 'D', 'E', 'F'];
    const P = { r: [360, 56], l: [180, 178], q: [540, 178], C: [90, 300], D: [270, 300], E: [450, 300], F: [630, 300] };
    const E = [['r', 'l'], ['r', 'q'], ['l', 'C'], ['l', 'D'], ['q', 'E'], ['q', 'F']];
    const rest = ['D, then (E F)', '(E F)', 'F', 'nothing: fun () -> Nil'];
    const base = (seen, cur, path) => {
      const it = {
        cap1: { kind: 'label', x: 40, y: 40, text: 'the melody', cls: 'cap' },
        cap2: { kind: 'label', x: 760, y: 40, text: 'what we have received', cls: 'cap' }
      };
      E.forEach(([a, b]) => { it['e' + a + b] = { kind: 'edge', x1: P[a][0], y1: P[a][1], x2: P[b][0], y2: P[b][1], t1: 30, t2: 34, cls: notes.indexOf(b) >= 0 && notes.indexOf(b) < seen ? 'used' : '' }; });
      ['r', 'l', 'q'].forEach(s => { it['s' + s] = { kind: 'leaf', x: P[s][0], y: P[s][1], text: 'Seq', cls: path.indexOf(s) >= 0 ? 'seed' : '' }; });
      notes.forEach((p, i) => { it['n' + p] = { kind: 'node', x: P[p][0], y: P[p][1], text: p, cls: i === cur ? 'fn cur' : i < seen ? 'used' : '' }; });
      for (let i = 0; i < seen; i++) it['c' + i] = { kind: 'chip', x: 810 + i * 110, y: 120, w: 84, h: 50, text: notes[i], cls: 'val kept' };
      return it;
    };
    const seal = (i, cls, sub) => ({ kind: 'seal', x: 1040, y: 280, w: 520, h: 96, text: 'k: ' + rest[i], sub, cls });
    const push = (it, what, seen, calls) => frames.push({ items: it, what, counters: [['notes visited', seen], ['we have called k', calls, calls === 1 ? 'time' : 'times']] });
    push(base(0, -1, []), 'to_seq m is a thunk: nothing has run, and no note is visited', 0, 0);
    const paths = [['r', 'l'], [], ['q'], []];
    for (let i = 0; i < 4; i++) {
      let it;
      if (paths[i].length) {
        it = base(i, -1, paths[i]);
        if (i) it.k = seal(i - 1, 'opening', 'called');
        push(it, i === 0 ? 'we call it: iter_k goes down to the left. At each Seq it builds a continuation that holds the right subtree'
          : 'we call k (): iter_k goes on in (E F), and builds a continuation that holds F', i, i);
      } else {
        it = base(i, -1, []);
        it.k = seal(i - 1, 'opening', 'called');
        push(it, 'we call k (): the traversal resumes where it stopped, at ' + notes[i], i, i);
      }
      it = base(i + 1, i, []);
      it['c' + i].cls = 'val kept new'; it['c' + i].from = { x: P[notes[i]][0], y: P[notes[i]][1] };
      it.k = seal(i, 'kept', 'not called: the traversal has stopped');
      it.k.from = { x: P[notes[i]][0], y: P[notes[i]][1] };
      push(it, 'f receives ' + notes[i] + ' and k. It does not call k: it returns Cons (' + notes[i] + ', k), and every call of iter_k has returned', i + 1, i);
    }
    const it = base(4, -1, []);
    it.k = seal(3, 'opening', 'called');
    push(it, 'we call k (): it is the function given at the start, and it returns Nil. The sequence has ended', 4, 4);
    return { frames, w: 1400, h: 370 };
  };

  MODES.star = function () {
    const s = ['a', 'a', 'a', 'b'], frames = [], X = i => 330 + i * 170, Y = 150, PX = [330, 500, 670], PY = 36;
    const pat = ['a*', 'a', 'b'];
    const base = (pos, part, ways, taken) => {
      const it = {
        cap0: { kind: 'label', x: 40, y: PY + 10, text: 'the pattern', cls: 'cap' },
        cap1: { kind: 'label', x: 40, y: Y + 10, text: 'the sequence', cls: 'cap' },
        cap2: { kind: 'label', x: 40, y: Y + 122, text: 'ways back, kept', cls: 'cap' }
      };
      pat.forEach((p, i) => { it['p' + i] = { kind: 'card', x: PX[i], y: PY, w: 130, h: 64, text: p, cls: i === part ? 'cur' : i < part ? 'done' : '' }; });
      s.forEach((c, i) => { it['c' + i] = { kind: 'pair', x: X(i), y: Y, text: c, cls: i === pos ? 'read' : '' }; });
      it.end = { kind: 'label', x: X(4) - 30, y: Y + 11, text: 'Nil', cls: 'mono' };
      s.forEach((c, i) => { it['q' + i] = { kind: 'edge', x1: X(i) + 40, y1: Y, x2: X(i + 1) - (i < 3 ? 66 : 70), y2: Y, cls: 'arrow ptr' }; });
      if (pos <= 4) {
        it.who = { kind: 'label', x: X(pos), y: Y - 62, text: 's', cls: 'mono reader' };
      }
      for (let i = 0; i < ways; i++)
        it['w' + i] = { kind: 'chip', x: X(i), y: Y + 112, w: 150, h: 50, text: 'fail', cls: i === taken ? 'val new' : 'ask' };
      return it;
    };
    const push = (it, what, back) => frames.push({ items: it, what, counters: [['we went back', back, back === 1 ? 'time' : 'times']] });
    push(base(0, 0, 0, -1), 'a*ab on the sequence a a a b: some a, then a, then b', 0);
    for (let i = 0; i < 3; i++) {
      const it = base(i + 1, 0, i + 1, -1);
      it['w' + i].from = { x: X(i), y: Y };
      push(it, 'the star takes the a at position ' + i + ', and keeps a way back: a fail that ends the star at position ' + i, 0);
    }
    push(base(3, 1, 3, -1), 'at position 3 there is b: the star ends, and we go on with the rest of the pattern', 0);
    push(base(3, 1, 3, 2), 'a does not match b. We call fail: it is the last way back that we kept', 1);
    push(base(2, 1, 2, -1), 'we are at position 2 again, and the sequence has not changed. The star ends here: it has taken two a', 1);
    push(base(3, 2, 2, -1), 'a matches the a at position 2', 1);
    push(base(4, 3, 2, -1), 'b matches: the answer is true. Two ways back were kept and never called', 1);
    return { frames, w: 1400, h: 300 };
  };

  // additions made when the first m numbers are read and every rest is a thunk
  function unshared(m) {
    let adds = 0;
    const plus = (a, b) => { adds++; return a + b; };
    const zip = (a, b) => () => { const A = a(), B = b(); return { x: plus(A.x, B.x), r: zip(A.r, B.r) }; };
    const tail = s => () => s().r();
    const fibs = () => ({ x: 0, r: () => ({ x: 1, r: zip(fibs, tail(fibs)) }) });
    let s = fibs; for (let i = 0; i < m; i++) { const c = s(); s = c.r; }
    return adds;
  }

  MODES.fibs = function (o) {
    const n = o.n || 8, frames = [], Y = 140, U = 158, X = i => 120 + i * U;
    const f = [0, 1]; for (let i = 2; i < n; i++) f.push(f[i - 1] + f[i - 2]);
    const it = {};
    const chain = m => {                                     // m cells built, then the rest, sealed
      for (let i = 0; i < m; i++) {
        it['c' + i] = { kind: 'pair', x: X(i), y: Y, text: String(f[i]), cls: (it['c' + i] || {}).cls || '' };
        it['p' + i] = { kind: 'edge', x1: X(i) + 42, y1: Y, x2: X(i + 1) - (i + 1 < m ? 58 : 66), y2: Y, cls: 'arrow ptr' };
      }
      it.rest = { kind: 'seal', x: X(m) + 4, y: Y, w: 124, h: 64, text: '…', sub: '', cls: '' };
    };
    chain(2);
    const push = (what, k) => frames.push({ items: clone(it), what, counters: [['additions, every cell kept', k], ['every cell computed again', unshared(k + 2)]] });
    push('0 and 1 are written. The rest of the list is not computed', 0);
    for (let i = 2; i < n; i++) {
      for (let j = 0; j < i; j++) it['c' + j] = { kind: 'pair', x: X(j), y: Y, text: String(f[j]), cls: j === i - 2 || j === i - 1 ? 'read' : '' };
      chain(i + 1);
      it['c' + i].cls = 'new';
      const px = (X(i - 2) + X(i - 1)) / 2, py = Y + 160;
      it['c' + i].from = { x: px, y: py };
      it.r1 = { kind: 'label', x: X(i - 2), y: Y - 78, text: 'fibs', cls: 'mono reader' };
      it.r2 = { kind: 'label', x: X(i - 1), y: Y - 78, text: 'tail fibs', cls: 'mono reader' };
      it.d1 = { kind: 'edge', x: X(i - 2), y: Y, x1: 0, y1: -64, x2: 0, y2: -38, cls: 'wire arrow' };
      it.d2 = { kind: 'edge', x: X(i - 1), y: Y, x1: 0, y1: -64, x2: 0, y2: -38, cls: 'wire arrow' };
      it.a1 = { kind: 'edge', x: X(i - 2), y: Y, x1: 0, y1: 36, x2: px - X(i - 2), y2: py - Y, t2: 30, cls: 'wire' };
      it.a2 = { kind: 'edge', x: X(i - 1), y: Y, x1: 0, y1: 36, x2: px - X(i - 1), y2: py - Y, t2: 30, cls: 'wire' };
      it.plus = { kind: 'node', x: px, y: py, text: '+', cls: 'op' };
      it.a3 = { kind: 'edge', x: px, y: py, x1: 0, y1: 0, x2: X(i) - px, y2: Y + 38 - py, t1: 30, t2: 6, cls: 'wire arrow made' };
      push('cell ' + i + ' is asked for. The two cells it reads are already there: one addition, ' + f[i - 2] + ' + ' + f[i - 1] + ' = ' + f[i], i - 1);
      it['c' + i].cls = ''; delete it['c' + i].from;
    }
    return { frames, w: Math.max(1400, X(n) + 110), h: 350 };
  };

  // ---- drawing --------------------------------------------------------------------------
  const Z = { edge: 0, card: 1, pair: 1, seal: 1, leaf: 2, node: 2, label: 3, chip: 4 };

  function mount(el, opt) {
    if (!el) return { finish() {} };
    const m = MODES[opt.mode](opt), frames = m.frames, n = frames.length - 1;
    const W = opt.w || m.w, H = opt.h || m.h, uid = 'sq' + (++serial);
    el.classList.add('anim', 'seqviz', 'sq-' + opt.mode);
    el.innerHTML = (frames[0].expr != null ? '<div class="sq-expr"></div>' : '') + '<svg class="sq-svg" viewBox="0 0 ' + W + ' ' + H + '"><defs>'
      + '<marker id="' + uid + '-ah" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="7" markerHeight="7" orient="auto"><path d="M0,1 L9,5 L0,9 z" class="sq-head"/></marker>'
      + '<pattern id="' + uid + '-hatch" width="12" height="12" patternUnits="userSpaceOnUse" patternTransform="rotate(45)"><rect width="12" height="12" class="sq-hatch-bg"/><rect width="5" height="12" class="sq-hatch-fg"/></pattern>'
      + '<filter id="' + uid + '-sh" x="-20%" y="-20%" width="140%" height="160%"><feDropShadow dx="0" dy="3" stdDeviation="4" flood-color="#23373B" flood-opacity=".16"/></filter>'
      + '</defs><g class="sq-body"></g></svg>'
      + '<div class="an-ctl sq-ctl"><div><button class="run sq-step">Step</button> <button class="run sq-run">Run</button> <button class="run sq-reset">Reset</button></div><div class="sq-count"></div></div>'
      + '<div class="an-what sq-what"></div>';
    const q = x => el.querySelector(x), body = q('.sq-body'), nodes = {};
    const mk = (tag, parent, cls) => { const e = parent.appendChild(document.createElementNS(NS, tag)); if (cls) e.setAttribute('class', cls); return e; };
    const first = {};
    frames.forEach(f => Object.keys(f.items).forEach(id => { if (!first[id]) first[id] = f.items[id]; }));
    Object.keys(first).sort((a, b) => Z[first[a].kind] - Z[first[b].kind]).forEach(id => {
      const it = first[id], g = mk('g', body), nd = { g };
      if (it.kind === 'edge') nd.line = mk('line', g);
      else if (it.kind === 'node') { nd.halo = mk('circle', g, 'sq-halo'); nd.circle = mk('circle', g, 'sq-shape'); nd.text = mk('text', g); nd.bg = mk('rect', g, 'sq-badge'); nd.bt = mk('text', g, 'sq-badge-t'); }
      else if (it.kind === 'pair') { nd.rect = mk('rect', g, 'sq-shape'); nd.div = mk('line', g, 'sq-div'); nd.text = mk('text', g); nd.dot = mk('circle', g, 'sq-dot'); }
      else if (it.kind === 'seal') { nd.rect = mk('rect', g, 'sq-shape'); nd.text = mk('text', g); nd.sub = mk('text', g, 'sq-sub'); }
      else if (it.kind === 'label') nd.text = mk('text', g);
      else { nd.rect = mk('rect', g, 'sq-shape'); nd.text = mk('text', g); }
      g.style.opacity = 0;
      nodes[id] = nd;
    });
    const place = (nd, x, y) => { nd.g.style.transform = 'translate(' + x + 'px,' + y + 'px)'; };
    const box = (r, w, h, rx) => { r.setAttribute('x', -w / 2); r.setAttribute('y', -h / 2); r.setAttribute('width', w); r.setAttribute('height', h); r.setAttribute('rx', rx); };
    let k = 0, timer = null, shown = {};
    const stop = () => { if (timer) { clearInterval(timer); timer = null; } };
    function render(quick) {
      const f = frames[k], now = {};
      el.classList.toggle('quick', !!quick);
      Object.keys(nodes).forEach(id => {
        const it = f.items[id], nd = nodes[id];
        if (!it) { nd.g.style.opacity = 0; return; }
        now[id] = true;
        nd.g.setAttribute('class', 'sq-it sq-' + it.kind + ' ' + (it.cls || ''));
        if (it.kind === 'edge') {
          const dx = it.x2 - it.x1, dy = it.y2 - it.y1, d = Math.hypot(dx, dy) || 1, a = it.t1 || 0, b = it.t2 || 0;
          nd.line.setAttribute('x1', it.x1 + dx / d * a); nd.line.setAttribute('y1', it.y1 + dy / d * a);
          nd.line.setAttribute('x2', it.x2 - dx / d * b); nd.line.setAttribute('y2', it.y2 - dy / d * b);
          if (/arrow/.test(it.cls)) nd.line.setAttribute('marker-end', 'url(#' + uid + '-ah)'); else nd.line.removeAttribute('marker-end');
        } else if (it.kind === 'node') {
          const r = /op/.test(it.cls) ? 26 : 30;
          nd.circle.setAttribute('r', r); nd.halo.setAttribute('r', r + 11);
          nd.circle.setAttribute('filter', 'url(#' + uid + '-sh)');
          nd.text.textContent = it.text; nd.text.setAttribute('y', 12);
          const b = it.badge != null;
          nd.bg.style.opacity = nd.bt.style.opacity = b ? 1 : 0;
          if (b) {
            const w = it.badge.length * 17 + 26;
            const bx = it.bside === 'left' ? -30 - w : 30;
            nd.bg.setAttribute('x', bx); nd.bg.setAttribute('y', -50); nd.bg.setAttribute('width', w); nd.bg.setAttribute('height', 38); nd.bg.setAttribute('rx', 19);
            nd.bg.setAttribute('class', 'sq-badge ' + it.bcls); nd.bt.setAttribute('class', 'sq-badge-t ' + it.bcls);
            nd.bt.textContent = it.badge; nd.bt.setAttribute('x', bx + w / 2); nd.bt.setAttribute('y', -22);
          }
        } else if (it.kind === 'leaf') {
          box(nd.rect, Math.max(56, it.text.length * 19 + 22), 52, 10);
          nd.text.textContent = it.text; nd.text.setAttribute('y', 11);
        } else if (it.kind === 'pair') {
          box(nd.rect, 116, 64, 10); nd.rect.setAttribute('filter', 'url(#' + uid + '-sh)');
          nd.div.setAttribute('x1', 18); nd.div.setAttribute('x2', 18); nd.div.setAttribute('y1', -32); nd.div.setAttribute('y2', 32);
          nd.text.textContent = it.text; nd.text.setAttribute('x', -20); nd.text.setAttribute('y', 12);
          nd.dot.setAttribute('cx', 38); nd.dot.setAttribute('cy', 0); nd.dot.setAttribute('r', 6);
        } else if (it.kind === 'seal') {
          box(nd.rect, it.w, it.h, 12);
          nd.rect.style.fill = /kept|called/.test(it.cls) ? '' : 'url(#' + uid + '-hatch)';
          nd.text.textContent = it.text; nd.text.setAttribute('y', it.sub ? -2 : 11);
          nd.sub.textContent = it.sub || ''; nd.sub.setAttribute('y', 24);
        } else if (it.kind === 'label') {
          nd.text.textContent = it.text;
        } else {
          box(nd.rect, it.w, it.h, it.kind === 'chip' ? it.h / 2 : 14);
          if (it.kind === 'card') nd.rect.setAttribute('filter', 'url(#' + uid + '-sh)');
          nd.text.textContent = it.text; nd.text.setAttribute('y', it.kind === 'chip' ? 10 : 11);
        }
        const x = it.x || 0, y = it.y || 0;
        if (!shown[id] && it.from && !quick) {                       // it comes out of the place it is made in
          nd.g.style.transition = 'none'; place(nd, it.from.x, it.from.y); nd.g.style.opacity = 0;
          void nd.g.getBoundingClientRect();
          nd.g.style.transition = '';
        }
        place(nd, x, y); nd.g.style.opacity = 1;
      });
      shown = now;
      q('.sq-count').innerHTML = f.counters.map(c => c[0] + ' <b>' + c[1] + '</b>' + (c[2] ? ' ' + c[2] : '')).join('<span class="sq-sep"></span>');
      q('.sq-what').textContent = f.what;
      if (f.expr != null) q('.sq-expr').innerHTML = (k ? '<span class="sq-eq">=</span>' : '<span class="sq-eq"></span>') + f.expr;
    }
    q('.sq-step').onclick = () => { stop(); if (k < n) k++; render(); };
    q('.sq-run').onclick = () => { stop(); if (k >= n) { k = 0; shown = {}; render(true); } timer = setInterval(() => { if (k >= n) return stop(); k++; render(); }, opt.every || 1400); };
    q('.sq-reset').onclick = () => { stop(); k = 0; shown = {}; render(true); };
    k = Math.min(opt.start || 0, n); render(true);
    return { finish() { stop(); k = n; render(true); }, frames: n + 1 };
  }
  window.SeqViz = { mount, unshared };
})();
