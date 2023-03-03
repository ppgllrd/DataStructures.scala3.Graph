package data.structures.mutable.disjointSet

import data.structures.mutable.disjointSet.indexedSet.IndexedSet

/**
 * Implementation of disjoint sets as described for Rem's implementation in:
 * Experiments on Union-Find Algorithms for the Disjoint-Set Data Structure.
 * Md. Mostofa Ali Patwary1, Jean Blair2, and Fredrik Manne
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

    while (iRootParent != jRootParent) {
      if (iRootParent < jRootParent) {
        if (jRoot == jRootParent) {
          return false
        }
        val jRootParentParent = parents(jRootParent)
        parents(jRoot) = jRootParentParent
        jRoot = jRootParent
        jRootParent = jRootParentParent

      } else {
        if (iRoot == iRootParent) {
          return false
        }
        val iRootParentParent = parents(iRootParent)
        parents(iRoot) = iRootParentParent
        iRoot = iRootParent
        iRootParent = iRootParentParent
      }
    }
    true
  }

  def union(x: A, y: A): Unit =
    indexUnion(indexOf(x), indexOf(y))

  protected final def indexUnion(i: Int, j: Int): Unit = {
    validate(i)
    validate(j)

    var iRoot = i
    var jRoot = j

    var iRootParent = parents(iRoot)
    var jRootParent = parents(jRoot)

    while (true) {
      if (iRootParent == jRootParent) {
        return
      }
      else if (iRootParent < jRootParent) {
        parents(jRoot) = iRootParent

        if (jRoot == jRootParent) {
          nComponents -= 1
          return
        }
        jRoot = jRootParent
        jRootParent = parents(jRoot)

      } else {
        parents(iRoot) = jRootParent

        if (iRoot == iRootParent) {
          nComponents -= 1
          return
        }
        iRoot = iRootParent
        iRootParent = parents(iRoot)
      }
    }
  }
}


object InterleavedDisjointSet {
  /** Constructs a disjoint set of using Rem's implementation in:
   * Experiments on Union-Find Algorithms for the Disjoint-Set Data Structure.
   * Md. Mostofa Ali Patwary1, Jean Blair2, and Fredrik Manne
   *
   * @param indexedSet the indexed set with elements for constructing new disjoint set.
   * @return a new disjoint set for provided elements.
   */
  def fromIndexedSet[A](indexedSet: IndexedSet[A]): InterleavedDisjointSet[A] =
    new FromIndexedSet[A](indexedSet) with InterleavedDisjointSet[A]
}
    
