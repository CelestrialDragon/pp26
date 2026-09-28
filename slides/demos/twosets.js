// Two ways to be a set — the code the slides of part 2 run, in the order of the slides.
// node twosets.js prints every "Run" output; the last block runs one client against both ways.

// ---- One interface: empty, insert, contains, union ------------------------

// Way 1: a set is DATA; every operation inspects it
const empty1    = [];
const insert1   = (s, n) => s.includes(n) ? s : [...s, n];
const contains1 = (s, x) => s.includes(x);
const union1    = (s, t) => [...s, ...t.filter(x => !s.includes(x))];   // inspects BOTH

// Way 2: a set IS A FUNCTION; every operation calls it
const empty2    = x => false;
const insert2   = (s, n) => (x => x === n || s(x));
const contains2 = (s, x) => s(x);
const union2    = (s, t) => (x => s(x) || t(x));                         // can only CALL s and t

// Same client, same answers
const a1 = insert1(insert1(empty1, 3), 5),  b1 = insert1(empty1, 4);
const a2 = insert2(insert2(empty2, 3), 5),  b2 = insert2(empty2, 4);
const test1 = [3, 4, 5, 6].map(x => contains1(union1(a1, b1), x));
const test2 = [3, 4, 5, 6].map(x => contains2(union2(a2, b2), x));
console.log("client   way 1:", test1, " way 2:", test2);

// ---- Extension 1: a new operation, isEmpty ----------------------------------

const isEmpty1 = s => s.length === 0;                                     // one line, nothing else touched

// const isEmpty2 = s => ???   a function cannot be asked whether it ever answers true
// ... unless a set carries two functions: every maker is rebuilt
const empty2b  = { contains: x => false,
                   isEmpty:  () => true };
const insert2b = (s, n) => ({
  contains: x => x === n || s.contains(x),
  isEmpty:  () => false });
const union2b  = (s, t) => ({
  contains: x => s.contains(x) || t.contains(x),
  isEmpty:  () => s.isEmpty() && t.isEmpty() });
const isEmpty2   = s => s.isEmpty();
const contains2b = (s, x) => s.contains(x);

const e1 = [empty1, insert1(empty1, 3), union1(empty1, empty1)].map(isEmpty1);
const e2 = [empty2b, insert2b(empty2b, 3), union2b(empty2b, empty2b)].map(isEmpty2);
console.log("isEmpty  way 1:", e1, " way 2:", e2);

// ---- Extension 2: an operation on two sets, equal -----------------------------

const equal1 = (s, t) => s.length === t.length && s.every(x => t.includes(x));   // inspects both
// const equal2 = (s, t) => ???   no finite number of calls decides whether two functions agree everywhere
console.log("equal    way 1:", equal1(union1(a1, b1), insert1(insert1(insert1(empty1, 4), 5), 3)), " way 2: ?");

// ---- Extension 3: a new kind of set, the even numbers ---------------------------

// Way 1: two kinds, told apart by a tag; every operation begins by reading it
const finite1    = xs => ({ kind: 'finite', xs });
const evens1     = { kind: 'evens' };
const contains1t = (s, x) =>
  s.kind === 'evens' ? x % 2 === 0
                     : s.xs.includes(x);
const isEmpty1t  = s =>
  s.kind === 'evens' ? false
                     : s.xs.length === 0;

// Way 2: one line, and it holds every even number
const evens2 = { contains: x => x % 2 === 0, isEmpty: () => false };
const test3 = [3, 4, 5, 6].map(x => contains2b(union2b(insert2b(insert2b(empty2b, 3), 5), evens2), x));
console.log("evens    way 2:", test3);

// ---- Extension 3, continued: the union of two kinds ------------------------------

// finite ∪ finite is finite. finite ∪ evens is neither kind: the operation keeps
// what it cannot compute as a third kind, and every operation gains a case.
const union1t = (s, t) =>
  s.kind === 'finite' && t.kind === 'finite'
    ? finite1([...s.xs, ...t.xs.filter(x => !s.xs.includes(x))])
    : { kind: 'union', s, t };
const contains1u = (s, x) =>
  s.kind === 'evens'  ? x % 2 === 0 :
  s.kind === 'union'  ? contains1u(s.s, x) || contains1u(s.t, x) :     // union2, as data
                        s.xs.includes(x);
const isEmpty1u = s =>
  s.kind === 'evens'  ? false :
  s.kind === 'union'  ? isEmpty1u(s.s) && isEmpty1u(s.t) :
                        s.xs.length === 0;
const equal1u = (s, t) => {
  if (s.kind === 'union' || t.kind === 'union') throw new Error("equal on a union: needs a normal form");
  if (s.kind === 'evens' || t.kind === 'evens') return s.kind === t.kind;
  return equal1(s.xs, t.xs);
};
const test4 = [3, 4, 5, 6].map(x => contains1u(union1t(finite1([3, 5]), evens1), x));
console.log("evens    way 1:", test4, " kind of {3,5} ∪ evens:", union1t(finite1([3, 5]), evens1).kind);

// ---- The check the slides rely on: one client, both ways, same answers -------------
// (a duplicate insert, an empty union, a union with the evens)
const client = (empty, insert, union, contains, isEmpty, evens) => {
  const s = insert(insert(insert(empty, 3), 5), 3);        // 3 inserted twice
  const u = union(s, evens);
  return [isEmpty(empty), isEmpty(s), isEmpty(union(empty, empty)),
          ...[3, 4, 5, 6].map(x => contains(s, x)), ...[3, 4, 5, 6].map(x => contains(u, x))];
};
const r1 = client(finite1([]), (s, n) => contains1u(s, n) ? s : finite1([...s.xs, n]), union1t, contains1u, isEmpty1u, evens1);
const r2 = client(empty2b, insert2b, union2b, contains2b, isEmpty2, evens2);
const same = JSON.stringify(r1) === JSON.stringify(r2);
console.log("same client, both ways:", r1.join(' '));
console.log(same ? "OK: both ways agree" : "MISMATCH: way 2 gave " + r2.join(' '));
if (!same) process.exit(1);
