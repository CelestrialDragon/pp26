// Way 1 in Java: sealed interface + records + pattern switch (Java 21+).
// java SetWay1.java
sealed interface Set permits Empty, Insert, Evens, Union {}
record Empty() implements Set {}
record Insert(int n, Set r) implements Set {}
record Evens() implements Set {}
record Union(Set s, Set t) implements Set {}   // a union the operation could not compute, kept as data

class SetWay1 {
  static boolean contains(Set s, int x) {
    return switch (s) {
      case Empty e              -> false;
      case Insert(int n, Set r) -> x == n || contains(r, x);
      case Evens e              -> x % 2 == 0;
      case Union(Set a, Set b)  -> contains(a, x) || contains(b, x);   // Way 2's union, as data
    };
  }
  static boolean isEmpty(Set s) {
    return switch (s) {
      case Empty e             -> true;
      case Insert i            -> false;
      case Evens e             -> false;
      case Union(Set a, Set b) -> isEmpty(a) && isEmpty(b);
    };
  }
  // two finite sets are merged; anything else is kept as a Union
  static Set union(Set s, Set t) {
    return switch (s) {
      case Empty e              -> t;
      case Insert(int n, Set r) -> new Insert(n, union(r, t));
      default                   -> new Union(s, t);
    };
  }
  static java.util.List<Integer> elements(Set s) {
    return switch (s) {
      case Empty e              -> java.util.List.of();
      case Insert(int n, Set r) -> { var l = new java.util.ArrayList<>(elements(r)); l.add(n); yield l; }
      default                   -> throw new IllegalArgumentException("not finite");
    };
  }
  // equal inspects both. Finite against finite: elements. Evens: tags. A Union: the open cell.
  static boolean equal(Set s, Set t) {
    if (s instanceof Union || t instanceof Union) throw new UnsupportedOperationException("equal on a Union: needs a normal form");
    if (s instanceof Evens || t instanceof Evens) return s.getClass() == t.getClass();
    return elements(s).stream().allMatch(x -> contains(t, x)) && elements(t).stream().allMatch(x -> contains(s, x));
  }
  public static void main(String[] a) {
    Set s = new Insert(3, new Insert(5, new Insert(3, new Empty())));   // 3 inserted twice
    Set u = union(s, new Evens());
    System.out.println(isEmpty(new Empty()) + " " + isEmpty(s) + " " + isEmpty(union(new Empty(), new Empty())));
    for (int x : new int[]{3, 4, 5, 6}) System.out.print(contains(s, x) + " "); System.out.println();
    for (int x : new int[]{3, 4, 5, 6}) System.out.print(contains(u, x) + " "); System.out.println();
    System.out.println(equal(s, new Insert(5, new Insert(3, new Empty()))) + " " + equal(new Evens(), s));
  }
}
