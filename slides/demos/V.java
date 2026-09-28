// Visitor: pattern matching by hand. java V.java
interface Visitor<R> { R onEmpty(); R onInsert(Set s, int n); }
abstract class Set { abstract <R> R accept(Visitor<R> v); }
class Empty extends Set {
  <R> R accept(Visitor<R> v) { return v.onEmpty(); }
}
class Insert extends Set {
  final Set s; final int n;
  Insert(Set s, int n) { this.s = s; this.n = n; }
  <R> R accept(Visitor<R> v) { return v.onInsert(s, n); }
}
class V {
  static boolean isEmpty(Set s) {          // one visitor, written outside the classes
    return s.accept(new Visitor<Boolean>() {
      public Boolean onEmpty() { return true; }
      public Boolean onInsert(Set r, int n) { return false; }
    });
  }
  public static void main(String[] a) {
    System.out.println(isEmpty(new Empty()) + " " + isEmpty(new Insert(new Insert(new Empty(), 3), 5)));
  }
}
