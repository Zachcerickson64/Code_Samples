
//Imported to use NoSuchElementException
import java.util.NoSuchElementException;

public class LinkedMinPQ<T extends Comparable<T>> implements MinPQ<T> {

  ////////////////////////
  // Instance variables //
  ////////////////////////
  Node top;
  int n;


  /////////////////////////////////
  // Node inner class definition //
  /////////////////////////////////

  class Node {
    T info;
    Node rchild;
    Node lchild;
    Node parent;

    //Node Constructor
    Node(T info, Node rchild, Node lchild, Node parent){
      this.info = info;
      this.rchild = rchild;
      this.lchild = lchild;
      this.parent = parent;
    }
  }

  //Priority Queue Constructor, intial size 0.
  LinkedMinPQ(){
    this.top = null;
    this.n = 0;
  }

  //////////////////////////////////////////////////////
  //     Methods you *might* want to implement.       //
  // There could be others, but these are some ideas. //
  //////////////////////////////////////////////////////

  // Helper method used to find the last node in the tree.
  // Returns a string with instructions on whether to go right or left from top.

  public static String path(int s){
    String path = "";
    while (s > 1){
      if (s % 2 == 0){
        path = "L" + path;
        s = s/2;
      }
      else{
        path = "R" + path;
        s = s/2;
      }
    }
    return path;
  }

  // Method that sends info from a higher node to a lower node
  // so that minimum priority maintains at the top.

  public void sink (Node d){

    while (d.lchild != null){           // Will stop when there is no node beneath it

      Node j = d.lchild;

        if(d.rchild != null){           // If a right child exists

          if (j.info.compareTo(d.rchild.info) <= 0){      // If left child is less than or equal to
            j = d.lchild;                                 // right child, d.info will switch with left child
          }

          if (j.info.compareTo(d.rchild.info) > 0){       // If left child is greater than right child,
            j = d.rchild;                                 // d.info will switch with right child
          }
        }

        if (j.info.compareTo(d.info) >= 0){         // If d is still the smallest, it stays put
          break;
        }
        else{                             // Otherwise, the info for d and j switch.
          T temp = d.info;
          d.info = j.info;
          j.info = temp;

          d = j;                          // j is now the node being sunk
        }
      }


  }



  //    a method that will swim info up from the bottom to a node
  //    where it's bigger than its parent and smaller than its children

  public void swim(Node d){

    while (d != top && d.info.compareTo(d.parent.info) <  0){      // While d is not the top node and
      T temp = d.parent.info;                                      // d < its parent, d switches info
      d.parent.info = d.info;                                      // with its parent and then d's parent
      d.info = temp;                                               // continues to swim

      d = d.parent;
    }

  }


  // Freebie helper method you can call in toString().
  // You do not need to use this method, but you can if you like.
  // This method will return a String listing the info in all the nodes
  // in a level in a binary tree, from left to right.
  // It is recursive: it calls itself in order to get the next level.
  public String printThisLevel (Node root ,int level) {
    StringBuilder s = new StringBuilder();
    if (root == null) {
      return s.toString();
    }
    if (level == 1) {
      s.append( root.info.toString() + ",");              // Added a comma to make toString look better
    } else if (level > 1) {
      s.append( printThisLevel(root.lchild, level-1));
      s.append( printThisLevel(root.rchild, level-1));
    }
    return s.toString();
  }


  /////////////////////////////////////////////////////////
  // Methods you must implement from the PQ interface //
  /////////////////////////////////////////////////////////

  // Remove and return the min (top) element

  public T delMin(){

    if (n == 0){
      throw new NoSuchElementException("Empty Tree");     // If the tree is empty, nothing can be deleted
    }

    else if (n == 1){               // If the tree only has a top node, returns top's info
      T toReturn = top.info;        // and makes top null. Decreases size by 1.
      top = null;
      n--;
      return toReturn;
    }
    else{
      T toReturn = top.info;                   // Creates a temporary pointer to top and uses the
      Node temp = top;                         // path function to know where to find the bottom node
      String [] pathAr = path(n).split("");
      int pathNum = pathAr.length;
      for(int i = 0; i < pathNum; i++){
        if (pathAr[i].equals("L")){
          temp = temp.lchild;
        }
        else{
          temp = temp.rchild;
        }
      }
      top.info = temp.info;         // Switches bottom node's info with top

      if (temp.parent.lchild.equals(temp)) {    // Decides which child pointer becomes null.
        temp.parent.lchild = null;
      }
      else {
        temp.parent.rchild = null;
      }
      temp.parent = null;             // Fully deletes last node.
      sink(top);                      // Sinks the info from the last node from the top.
      n--;                            // Decreases size by 1 and returns the original top info.
      return toReturn;
    }

  }


  // Insert a new element.
  public void insert(T key){

    if (n == 0){
      Node newTop = new Node(key, null, null, null);      // Creates top node if tree is empty
      top = newTop;
    }
    else{
      Node temp = top;
      String [] pathAr = path(n+1).split("");            // Uses path to find where the next node should go
      int pathNum = pathAr.length;                       // Does this by finding path of size + 1
      for(int i = 0; i < pathNum; i++){
        if (pathAr[i].equals("L")){
          if (temp.lchild == null){                         // Once it finds the final node, if it is a left
            Node newNode = new Node(key, null, null, null); // child, assigns pointers then swims the info.
            newNode.parent = temp;
            temp.lchild = newNode;
            swim(newNode);
          }
          else{
            temp = temp.lchild;
          }
        }
        else{
          if (temp.rchild == null){                           // If it's a right child, does the same.
            Node newNode = new Node(key, null, null, null);
            newNode.parent = temp;
            temp.rchild = newNode;
            swim(newNode);
          }
          else{
            temp = temp.rchild;
          }
        }
      }
    }
    n++;        // Increases size by 1.

  }


  // Return true if the PQ is empty
  public boolean isEmpty(){
    return (n == 0);
  }


  // Return the size of the PQ.
  public int size(){
    return n;
  }


  // Return a string showing the PQ in level order, i.e.,
  // containing the info at each node, L to R, from top level to bottom.
  public String toString(){
    String [] pathAr = path(n).split("");         // Uses a for loop with printThisLevel()
    int levels = (pathAr.length)+1;               // and uses path to find the amount of levels.
    String tree = "";
    for (int i = 1; i<=levels; i++){
      tree = tree + printThisLevel(top, i);
    }
    return tree;
  }

  ////////////////////////////////////////////////////////////
  // Main method you must write to test out your code above //
  ////////////////////////////////////////////////////////////

  public static void main (String[] args) {

    MinPQ<Integer> myM = new LinkedMinPQ<Integer>();      // An integer Queue

    myM.insert(3);              //Inserts several integers while also prioritizing them
    myM.insert(4);
    myM.insert(2);
    myM.insert(1);
    myM.insert(8);
    myM.insert(11);
    myM.insert(7);
    myM.insert(8);

    System.out.format("My tree has elements: %s\n", myM.toString());  // My tree has elements: 1,2,3,4,8,11,7,8,

    System.out.println("The tree is size: " + myM.size());    // The tree is size: 8
    System.out.format("Removing: %s\n", myM.delMin());        // Removing: 1
    System.out.println("The tree is size: " + myM.size());    // The tree is size: 7
    System.out.format("Removing: %s\n", myM.delMin());        // Removing: 2
    System.out.println("The tree is size: " + myM.size());    // The tree is size: 6
    System.out.format("Removing: %s\n", myM.delMin());        // Removing: 3
    System.out.println("The tree is size: " + myM.size());    // The tree is size: 5
    System.out.format("Removing: %s\n", myM.delMin());        // Removing: 4
    System.out.println("The tree is size: " + myM.size());    // The tree is size: 4

    System.out.format("My tree has elements: %s\n", myM.toString()); // My tree has elements: 7,8,8,11,

    if (myM.isEmpty() == true){                   // The Tree is not Empty!
      System.out.println("The Tree is Empty!");
    }
    else{
      System.out.println("The Tree is not Empty!");
    }

    myM.delMin();
    myM.delMin();
    myM.delMin();
    myM.delMin();

    if (myM.isEmpty() == true){                         // The Tree is Empty!
      System.out.println("The Tree is Empty!");
    }
    else{
      System.out.println("The Tree is not Empty!");
    }

    MinPQ<String> myMs = new LinkedMinPQ<String>();     // A String Queue

    myMs.insert("Alex");      // Inserts names
    myMs.insert("Ben");
    myMs.insert("Grant");
    myMs.insert("Allen");
    myMs.insert("Tony");

    System.out.format("My tree has elements: %s\n", myMs.toString()); // My tree has elements: Alex,Allen,Grant,Ben,Tony,

    System.out.println("The tree is size: " + myMs.size());   // The tree is size: 5
    System.out.format("Removing: %s\n", myMs.delMin());       // Removing: Alex
    System.out.println("The tree is size: " + myMs.size());   // The tree is size: 4
    System.out.format("Removing: %s\n", myMs.delMin());       // Removing: Allen
    System.out.println("The tree is size: " + myMs.size());   // The tree is size: 3
    System.out.format("Removing: %s\n", myMs.delMin());       // Removing: Ben
    System.out.println("The tree is size: " + myMs.size());   // The tree is size: 2
    System.out.format("Removing: %s\n", myMs.delMin());       // Removing: Grant
    System.out.println("The tree is size: " + myMs.size());   // The tree is size: 1

    System.out.format("My tree has element: %s\n", myMs.toString());    // My tree has element: Tony,

    if (myMs.isEmpty() == true){                  // The Tree is not Empty!
      System.out.println("The Tree is Empty!");
    }
    else{
      System.out.println("The Tree is not Empty!");
    }

    myMs.delMin();

    if (myMs.isEmpty() == true){                  // The Tree is Empty!
      System.out.println("The Tree is Empty!");
    }
    else{
      System.out.println("The Tree is not Empty!");
    }





  }

}
