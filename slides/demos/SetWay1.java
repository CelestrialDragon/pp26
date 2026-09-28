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
  // equal inspects both sets, through a normal form: does the set hold the evens, and which
  // other elements does it hold? Every kind is covered, Union included.
  record Normal(boolean evens, java.util.SortedSet<Integer> rest) {}
  static boolean hasEvens(Set s) {
    return switch (s) {
      case Empty e              -> false;
      case Insert(int n, Set r) -> hasEvens(r);
      case Evens e              -> true;
      case Union(Set a, Set b)  -> hasEvens(a) || hasEvens(b);
    };
  }
  static void inserted(Set s, java.util.List<Integer> out) {
    switch (s) {
      case Empty e              -> {}
      case Insert(int n, Set r) -> { out.add(n); inserted(r, out); }
      case Evens e              -> {}
      case Union(Set a, Set b)  -> { inserted(a, out); inserted(b, out); }
    }
  }
  static Normal normal(Set s) {
    boolean e = hasEvens(s);
    var xs = new java.util.ArrayList<Integer>(); inserted(s, xs);
    var rest = new java.util.TreeSet<Integer>();
    for (int n : xs) if (!(e && n % 2 == 0)) rest.add(n);
    return new Normal(e, rest);
  }
  static boolean equal(Set s, Set t) { return normal(s).equals(normal(t)); }
  public static void main(String[] a) {
    Set s = new Insert(3, new Insert(5, new Insert(3, new Empty())));   // 3 inserted twice
    Set u = union(s, new Evens());
    System.out.println(isEmpty(new Empty()) + " " + isEmpty(s) + " " + isEmpty(union(new Empty(), new Empty())));
    for (int x : new int[]{3, 4, 5, 6}) System.out.print(contains(s, x) + " "); System.out.println();
    for (int x : new int[]{3, 4, 5, 6}) System.out.print(contains(u, x) + " "); System.out.println();
    System.out.println(equal(s, new Insert(5, new Insert(3, new Empty()))) + " " + equal(new Evens(), s) + " "
      + equal(new Evens(), new Insert(2, new Evens())) + " " + equal(union(s, new Evens()), new Insert(3, new Insert(5, new Evens()))));
  }
}
