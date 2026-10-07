// Session 5, block 1, part 1: the counter as an object.
//
//   java Counter.java        prints  1 2 1  then  1
//
// The slide has the same counter as a closure, in JavaScript. The two say the same thing:
// a state that nothing outside can reach, and the functions that can.
class Counter {
  // The state is private, as the variable that a closure captures. Two methods share it:
  // an object is several functions over one state, a closure is one function over its own.
  private int n = 0;
  int tick()   { return ++n; }
  void reset() { n = 0; }

  public static void main(String[] args) {
    // two objects: two counters, each with its own n
    Counter a = new Counter(), b = new Counter();
    System.out.println(a.tick() + " " + a.tick() + " " + b.tick());
    a.reset();                  // changes a's n only
    System.out.println(a.tick());
  }
}
