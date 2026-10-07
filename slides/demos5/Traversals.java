// Session 5, block 1, part 2: the usual traversals of a list.
//
//   java Traversals.java     prints  to be or not  three times
//
// The three visit every element, in order. They differ in who calls whom.
import java.util.*;

class Traversals {
  public static void main(String[] args) {
    List<String> words = List.of("to", "be", "or", "not");

    // 1. We give the list a function, and the list calls it on every element.
    //    The loop is inside forEach: we do not see it, and our function has no break.
    words.forEach(w -> System.out.print(w + " "));
    System.out.println();

    // 2. We ask the list for an iterator, and we call it. The loop is ours:
    //    we decide when to ask for the next element, and whether to ask at all.
    Iterator<String> it = words.iterator();
    while (it.hasNext()) {
      String w = it.next();
      System.out.print(w + " ");
    }
    System.out.println();

    // 3. The same as 2: the compiler writes the iterator and its loop for us.
    for (String w : words) System.out.print(w + " ");
    System.out.println();
  }
}
