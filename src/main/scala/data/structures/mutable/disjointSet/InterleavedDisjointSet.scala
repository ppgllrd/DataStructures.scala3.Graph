package data.structures.mutable.disjointSet

import data.structures.mutable.disjointSet.indexedSet.IndexedSet

/**
 * Implementation of disjoint sets as described for Rem's implementation in:
 * Experiments on Union-Find Algorithms for the Disjoint-Set Data Structure.
 *   Md. Mostofa Ali Patwary1, Jean Blair2, and Fredrik Manne
 * @tparam A type of elements in disjoint set.
 * @author Pepe Gallardo.          
 */
trait InterleavedDisjointSet[A] extends DisjointSet[A] {
  protected val parents: Array[Int] = Array.tabulate[Int](size)(i => i)
  // number of different components
  protected var nComponents: Int = size

  def numberOfComponents: Int = nComponents

  protected final def indexUnite(i: Int, j: Int, doUnion: Boolean): Boolean = {
    validate(i)
    validate(j)

    var iRoot = i
    var jRoot = j

    var iRootParent = parents(iRoot)
    var jRootParent = parents(jRoot)

    var canBeConnected = true
    while (canBeConnected && (iRootParent != jRootParent)) {
      if (iRootParent < jRootParent) {
        if (iRoot == iRootParent) {
          if (doUnion) {
            parents(iRoot) = jRootParent
            nComponents -= 1
          }
          canBeConnected = false
        } else {
          parents(iRoot) = jRootParent
          iRoot = iRootParent
          iRootParent = parents(iRoot)
        }
      } else {
        if (jRoot == jRootParent) {
          if (doUnion) {
            parents(jRoot) = iRootParent
            nComponents -= 1
          }
          canBeConnected = false
        } else {
          parents(jRoot) = iRootParent
          jRoot = jRootParent
          jRootParent = parents(jRoot)
        }
      }
    }
    canBeConnected
  }

  def areConnected(x: A, y: A): Boolean =
    indexUnite(indexOf(x), indexOf(y), false)

  def union(x: A, y: A): Unit =
    indexUnite(indexOf(x), indexOf(y), true)
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
    
