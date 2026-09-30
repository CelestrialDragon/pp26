// Session 5, block 1, part 4: external iteration on a list, and a list iterator written
// by hand.
//
//   java ListIter.java       prints  found=true visited=3
//                                    to be or not
//                                    next, at the end: java.util.NoSuchElementException
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

public class ListIter {
  // a list as session 2 drew it: a cell holds a value and the rest of the list
  record Node<T>(T value, Node<T> next) {}

  // An iterator is an object that remembers where we are in the list: `here` is its
  // state. It is the counter of part 1, with a position in a list in the place of a number.
  static class ListIterator<T> implements Iterator<T> {
    private Node<T> here;
    ListIterator(Node<T> l) { here = l; }
    public boolean hasNext() { return here != null; }
    public T next() {
      if (here == null) throw new NoSuchElementException();   // asked once too many
      T x = here.value();
      here = here.next();          // the iterator moves: the next call gives the next element
      return x;
    }
  }

  public static void main(String[] args) {
    // Is there a newline in the text? The question of part 3, with Java's own iterator.
    // We stop by not calling next again: three elements are visited, where fold visited five.
    List<Character> text = List.of('h', 'i', '\n', 'y', 'o');
    boolean found = false;
    int visited = 0;
    Iterator<Character> it = text.iterator();
    while (it.hasNext() && !found) {
      visited++;
      found = it.next() == '\n';
    }
    System.out.println("found=" + found + " visited=" + visited);

    // our iterator, on a list of our own cells
    Node<String> words = new Node<>("to", new Node<>("be", new Node<>("or", new Node<>("not", null))));
    Iterator<String> w = new ListIterator<>(words);
    while (w.hasNext()) System.out.print(w.next() + " ");
    System.out.println();
    // the iterator is at the end, and it stays there: one more next fails
    try { w.next(); } catch (NoSuchElementException e) { System.out.println("next, at the end: " + e); }
  }
}
