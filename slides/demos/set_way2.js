// Way 2, complete: a set IS ITS OPERATIONS — a record of two functions — and every operation
// calls it. The kinds live in the makers: empty, insert, union, evens.  node set_way2.js

const empty  = { contains: x => false,
                 isEmpty:  () => true };
const insert = (s, n) => ({
  contains: x => x === n || s.contains(x),
  isEmpty:  () => false });
const union  = (s, t) => ({
  contains: x => s.contains(x) || t.contains(x),
  isEmpty:  () => s.isEmpty() && t.isEmpty() });
const evens  = { contains: x => x % 2 === 0,
                 isEmpty:  () => false };

const contains = (s, x) => s.contains(x);       // the operation is just the call
const isEmpty  = s => s.isEmpty();

// const equal = (s, t) => ???
// s and t can only be called. No number of calls decides whether they agree on every x.

// the same lines as set_way2.ml and SetWay2.java print
const s = insert(insert(insert(empty, 3), 5), 3);          // 3 inserted twice
const u = union(s, evens);
console.log(isEmpty(empty), isEmpty(s), isEmpty(union(empty, empty)));
console.log(...[3, 4, 5, 6].map(x => contains(s, x)));
console.log(...[3, 4, 5, 6].map(x => contains(u, x)));
