package data.structures.mutable.disjointSet

import data.structures.mutable.disjointSet.indexedSet.IndexedSet

/**
 * Interleaved implementation of disjoint sets as described by Martin Rem using splicing as a compression technique.
 * Corresponds to RemSP algorithm presented in:
 * Experiments on Union-Find Algorithms for the Disjoint-Set Data Structure.
 * Md. Mostofa Ali Patwary, Jean Blair, and Fredrik Manne
 *
 * @tparam A type of elements in disjoint set.
 * @author Pepe Gallardo.          
 */
trait InterleavedDisjointSet[A] extends DisjointSet[A] {
  protected val parents: Array[Int] = Array.tabulate[Int](size)(i => i)
  // number of different components
  protected var nComponents: Int = size

  def numberOfComponents: Int = nComponents

  def areConnected(x: A, y: A): Boolean =
    indexAreConnected(indexOf(x), indexOf(y))

  protected final def indexAreConnected(i: Int, j: Int): Boolean = {
    validate(i)
    validate(j)

    var iRoot = i
    var jRoot = j

    var iRootParent = parents(iRoot)
    var jRootParent = parents(jRoot)

    // we ascend until we get to a common ancestor (elements are connected) or until we get to a root (elements are
    // not connected)
    while (iRootParent != jRootParent) {
      if (iRootParent < jRootParent) {
        if (jRoot == jRootParent) {
          return false
        }
        jRoot = jRootParent
        jRootParent = parents(jRoot)

      } else {
        if (iRoot == iRootParent) {
          return false
        }
        iRoot = iRootParent
        iRootParent = parents(iRoot)
      }
    }
    true
  }

  def union(x: A, y: A): Boolean =
    indexUnion(indexOf(x), indexOf(y))

  protected final def indexUnion(i: Int, j: Int): Boolean = {
    validate(i)
    validate(j)

    var iRoot = i
    var jRoot = j

    var iRootParent = parents(iRoot)
    var jRootParent = parents(jRoot)

    /*
    we ascend until we get to a common ancestor (elements were connected) or until we get to a root (elements were not
    connected). We connect elements in one set to elements in other sut while ascending using splicing. Splicing is
    explained in the reference above as follow:

    In the case when iRoot is to be moved to iRootParent it works as follows: just before this operation, iRoot is
    stored in a temporary variable z and then, just before moving iRoot up to its parent p(z), iRootParent is set to
    jRootParent, making the subtree rooted at iRoot a sibling of jRoot. This neither compromises the increasing parent
    property (because iRootParent < jRootParent) nor invalidates the set structures (because the two sets will have been
    merged when the operation ends). The eﬀect of splicing is that each new parent has a higher value than the value of
    the old parent, thus compressing the tree.
    */

    while (iRootParent != jRootParent) {
      if (iRootParent < jRootParent) {
        parents(jRoot) = iRootParent

        if (jRoot == jRootParent) {
          nComponents -= 1
          return true
        }
        jRoot = jRootParent
        jRootParent = parents(jRoot)

      } else {
        parents(iRoot) = jRootParent

        if (iRoot == iRootParent) {
          nComponents -= 1
          return true
        }
        iRoot = iRootParent
        iRootParent = parents(iRoot)
      }
    }
    false
  }
}


object InterleavedDisjointSet {
  /** Constructs an interleaved with splicing disjoint set using Martin Rem's implementation described in:
   * Experiments on Union-Find Algorithms for the Disjoint-Set Data Structure.
   * Md. Mostofa Ali Patwary, Jean Blair, and Fredrik Manne
   *
   * @param indexedSet the indexed set with elements for constructing new disjoint set.
   * @return a new disjoint set for provided elements.
   */
  def fromIndexedSet[A](indexedSet: IndexedSet[A]): InterleavedDisjointSet[A] =
    new FromIndexedSet[A](indexedSet) with InterleavedDisjointSet[A]
}
    
