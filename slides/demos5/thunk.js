const t = () => {
  console.log("computing");
  return 6 * 7;
};

console.log("the thunk is made");
const a = t();
const b = t();
console.log(a, b);
