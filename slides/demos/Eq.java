// Session 3, "Classes": equal as a method, in Java.     java Eq.java      prints  true false
// Way 2 bundles each operation with its kind, so equals is written in each class. An Insert
// compares itself with o only if o is an Insert too: it can look inside o because o is a set
// of its own class. Across kinds, a method sees the receiver only: the binary-method problem.
abstract class Set { abstract boolean contains(int x); }
class Empty extends Set {
  boolean contains(int x) { return false; }
  @Override public boolean equals(Object o) { return o instanceof Empty; }
}
class Insert extends Set {
  private final Set s; private final int n;
  Insert(Set s, int n) { this.s = s; this.n = n; }
  boolean contains(int x) { return x == n || s.contains(x); }
  @Override public boolean equals(Object o) {
    return o instanceof Insert i && i.n == n && i.s.equals(s);
  }
}
class Eq { public static void main(String[] a) {
  Set x = new Insert(new Empty(), 3), y = new Insert(new Empty(), 3), z = new Insert(new Empty(), 4);
  System.out.println(x.equals(y) + " " + x.equals(z)); } }
