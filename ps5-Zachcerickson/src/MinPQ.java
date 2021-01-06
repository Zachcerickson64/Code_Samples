public interface MinPQ<T extends Comparable<T>> {
  T delMin();
  void insert(T key);
  boolean isEmpty();
  int size();
  String toString();
}
