// A — by construction, in Java: permits is the "|", record is the "of"
sealed interface Shape permits Circle, Square, Tri {}
record Circle(double r) implements Shape {}
record Square(double s) implements Shape {}
record Tri(double b, double h) implements Shape {}

class A {
  // an operation, added from outside, touching none of the above
  static double area(Shape sh) {
    return switch (sh) {
      case Circle c -> Math.PI * c.r() * c.r();
      case Square s -> s.s() * s.s();
      // Tri deliberately missing
    };
  }
  public static void main(String[] a) { System.out.println(area(new Square(3))); }
}
