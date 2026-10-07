// Session 5, block 1, part 5: a generator, in JavaScript.
//
//   node generator.js       prints   { value: 'h', done: false } { value: 'i', done: false } { value: undefined, done: true }
//
// function* makes a generator function. Calling two() runs nothing: it returns an object
// with a method next. Each call of next runs the function from where it stopped to the next
// yield, and answers { value, done }. After the last yield, the function ends: done is true.
function* two() {
  yield 'h';
  yield 'i';
}

const g = two();
console.log(g.next(), g.next(), g.next());
