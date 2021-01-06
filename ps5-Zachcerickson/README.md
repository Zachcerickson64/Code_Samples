## Problem Set 5

### Due Thursday, October 18 @ 11:59pm

[Click here for an up-to-date version of this README](https://github.com/BC-CSCI-1102-F18-MW/ps5/blob/master/README.md) 

---
### SW 2.4.24 Minimum priority queue with explicit links

As we have seen, *complete binary trees* can be efficiently represented in a sequential array by allocating the elements of the tree in level-order in the array. Binary trees can also be represented with a linked data structures, as shown below, in which each node contains some value and has a pointer to the left child and a pointer to the right child.


```java
class Node {
                                           +-----------------+
  private T info;                          |       info      |
  private Node lchild;                     +--------+--------+
  private Node richild;                    | lchild | rchild |
  ...                                      +--------+--------+
}
```

In this problem set, you will implement a **minimum** priority queue using a **heap-ordered binary tree**, which you will implement with a **triply linked Node** structure instead of an array. You will need three links per node: two to traverse down the tree (one for the left child and one for the right child) and one to traverse up the tree to the node's parent. 

### Interface
Please use the following interface for your ADT (included in the `src` directory as `MinPQ.java`):

```java
public interface MinPQ<T extends Comparable<T>> {
  T delMin();
  void insert(T key);
  boolean isEmpty();
  int size();
  String toString();
}
```

Note that the specification of the generic type variable `T` in:

```java
T extends Comparable<T>
```

means that the type variable `T` can be replaced by any type that includes *at least* an `int compareTo(T other)` function. Recall that `compareTo` returns a positive number when the object calling it is larger than the argument, 0 if they're equal, and a negative number if the object calling is smaller than the argument.

### Getting started
I have included some skeleton code to get you started in the `LinkedMinPQ.java` file in the `src` directory. *You do not have to use this code*, but you should name your implementation `LinkedMinPQ.java`.

As with the sequential implementation, the `insert` operation must find the tree node to which a new entry is to be attached, i.e., the next available place to attach a new node. It should then "swim" that new entry up to a node where it is smaller than its children but bigger than its parent.

Similarly, the `delMin` operation, after it removes the top (minimum) node, must replace the top node with the last entry in the complete binary tree, and then "sink" it down to a location where it is smaller than its children but bigger than its parent. 

In the sequential implementation these tree locations were easy to find using the size of the tree to compute the appropriate array index. With this linked implementation, a little more work is required. You can still use the size of the tree, but you will use it to compute the path from the root to the desired node (i.e., whether to go right or left at each node, as you proceed down from the root). Integer division by 2 will come in handy, as will calculating the remainder when divided by 2 by using the modulus operator %.

### Testing your code
As usual, write a few unit tests in order to demonstrate that the code is working correctly. Remember: the top of the heap should be the **smallest** item (e.g., a small number, a letter earlier in the alphabet). You might find it easiest to test with Strings that are single letters so that you're putting things in alphabetical order, which is natural to interpret.

### Extra credit opportunity
If you implement your approach for finding the path from the root to a desired node **recursively** you will get 1 bonus point that you can add to any other problem set score, or 2 bonus points that you can add to Hour Exam 1.
