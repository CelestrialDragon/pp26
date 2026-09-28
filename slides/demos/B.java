// The row check: a kind that does not provide every operation. javac B.java fails; the slide quotes it.
abstract class Set {
  abstract boolean contains(int x);
  abstract boolean isEmpty();
}
class Empty extends Set {
  boolean contains(int x) { return false; }
  boolean isEmpty()       { return true; }
}
class Evens extends Set {
  boolean contains(int x) { return x % 2 == 0; }
  // isEmpty deliberately missing
}
