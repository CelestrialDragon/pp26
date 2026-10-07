// Session 5, block 1, part 5: the object that a compiler makes of the generator two.
//
//   node generator_object.js   prints the same three answers as generator.js
//
// The same function two, written by hand. Its local state is pc, the position in the code
// where next() resumes. Each yield of the generator is one case: it writes in pc where to
// resume, and returns the element. The end of the function is the default case: done is
// true. Compilers of generators to older JavaScript (regenerator) produce this shape.
const two = () => {
  let pc = 0;  // where next() resumes
  const next = () => {
    switch (pc) {
      case 0: pc = 1; return { value: 'h', done: false };  // yield 'h'
      case 1: pc = 2; return { value: 'i', done: false };  // yield 'i'
      default: return { value: undefined, done: true };    // the end
    }
  };
  return { next };
};

const g = two();
console.log(g.next(), g.next(), g.next());
