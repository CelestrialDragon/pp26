// Way 2 in Java: an abstract class is the interface; each subclass is one kind, with its own operations.
// java SetWay2.java
abstract class Set {
  abstract boolean contains(int x);
  abstract boolean isEmpty();
}
class Empty extends Set {
  boolean contains(int x) { return false; }
  boolean isEmpty()       { return true; }
}
class Insert extends Set {
  private final Set s; private final int n;
  Insert(Set s, int n) { this.s = s; this.n = n; }
  boolean contains(int x) { return x == n || s.contains(x); }
  boolean isEmpty()       { return false; }
}
class Evens extends Set {
  boolean contains(int x) { return x % 2 == 0; }
  boolean isEmpty()       { return false; }
}
class Union extends Set {
  private final Set s, t;
  Union(Set s, Set t) { this.s = s; this.t = t; }
  boolean contains(int x) { return s.contains(x) || t.contains(x); }
  boolean isEmpty()       { return s.isEmpty() && t.isEmpty(); }
}
// static boolean equal(Set s, Set t) { ??? }   s and t can only be called; never compared.

class SetWay2 {
  public static void main(String[] a) {
    Set s = new Insert(new Insert(new Insert(new Empty(), 3), 5), 3);   // 3 inserted twice
    Set u = new Union(s, new Evens());
    System.out.println(new Empty().isEmpty() + " " + s.isEmpty() + " " + new Union(new Empty(), new Empty()).isEmpty());
    for (int x : new int[]{3, 4, 5, 6}) System.out.print(s.contains(x) + " "); System.out.println();
    for (int x : new int[]{3, 4, 5, 6}) System.out.print(u.contains(x) + " "); System.out.println();
  }
}
