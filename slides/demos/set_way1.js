// Way 1, complete: a set is DATA — a record with a tag — and every operation inspects it.
// The kinds: finite (an array), evens, and union (two sets kept as they are).  node set_way1.js

const finite = xs => ({ kind: 'finite', xs });
const empty  = finite([]);
const evens  = { kind: 'evens' };

function contains(s, x) {
  if (s.kind === 'finite') return s.xs.includes(x);
  if (s.kind === 'evens')  return x % 2 === 0;
  if (s.kind === 'union')  return contains(s.s, x) || contains(s.t, x);
}
function isEmpty(s) {
  if (s.kind === 'finite') return s.xs.length === 0;
  if (s.kind === 'evens')  return false;
  if (s.kind === 'union')  return isEmpty(s.s) && isEmpty(s.t);
}
// two finite sets are merged; anything else is kept as a union
function union(s, t) {
  if (s.kind === 'finite' && t.kind === 'finite')
    return finite([...s.xs, ...t.xs.filter(x => !s.xs.includes(x))]);
  return { kind: 'union', s, t };
}
const insert = (s, n) => contains(s, n) ? s : union(s, finite([n]));

// equal inspects both sets, through a normal form: does the set hold the evens, and which
// other numbers does it hold? Two sets are equal when their normal forms are.
function hasEvens(s) { return s.kind === 'evens' || (s.kind === 'union' && (hasEvens(s.s) || hasEvens(s.t))); }
function listed(s)   { return s.kind === 'finite' ? s.xs : s.kind === 'union' ? [...listed(s.s), ...listed(s.t)] : []; }
function normal(s)   { const e = hasEvens(s); return [e, [...new Set(listed(s).filter(n => !(e && n % 2 === 0)))].sort((a, b) => a - b)]; }
const equal = (s, t) => JSON.stringify(normal(s)) === JSON.stringify(normal(t));

// the same lines as set_way1.ml and SetWay1.java print
const s = insert(insert(insert(empty, 3), 5), 3);          // 3 inserted twice
const u = union(s, evens);
console.log(isEmpty(empty), isEmpty(s), isEmpty(union(empty, empty)));
console.log(...[3, 4, 5, 6].map(x => contains(s, x)));
console.log(...[3, 4, 5, 6].map(x => contains(u, x)));
console.log(equal(s, insert(insert(empty, 5), 3)), equal(evens, s), equal(evens, insert(evens, 2)), equal(u, insert(insert(evens, 3), 5)));
