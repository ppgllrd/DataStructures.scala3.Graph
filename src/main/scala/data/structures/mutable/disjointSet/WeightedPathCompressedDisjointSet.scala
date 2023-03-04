package data.structures.mutable.disjointSet

import data.structures.mutable.disjointSet.indexedSet.IndexedSet

/**
 * Implementation of disjoint sets with weighted trees and path compression.
 * See: Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne.
 *
 * @tparam A type of elements in disjoint set.
 * @author Pepe Gallardo
 */
trait WeightedPathCompressedDisjointSet[A] extends DisjointSet[A] {
  protected val parents: Array[Int] = Array.tabulate[Int](size)(i => i)
  private val sizes = Array.fill[Int](size)(1)
  // number of different components
  protected var nComponents: Int = size

  def numberOfComponents: Int = nComponents

  final def areConnected(x: A, y: A): Boolean =
    findRoot(x) == findRoot(y)

  final def union(x: A, y: A): Boolean = {
    val xRoot = findRoot(x)
    val yRoot = findRoot(y)

    if (xRoot != yRoot) {
      val xSize = sizes(xRoot)
      val ySize = sizes(yRoot)

      // link smallest tree below large one
      // update size for new common root
      if (xSize < ySize) {
        parents(xRoot) = yRoot
        sizes(yRoot) += xSize
      } else {
        parents(yRoot) = xRoot
        sizes(xRoot) += ySize
      }
      nComponents -= 1
      true
    } else
      false
  }

  protected def findRoot(x: A): Int =
    findIndexRoot(indexOf(x))

  protected final def findIndexRoot(i: Int): Int = {
    validate(i)
    var root = i
    var stop = false
    while (!stop) {
      val parent = parents(root)
      if (root == parent)
        stop = true
      else
        root = parent
    }

    // path compression
    var j = i
    while (j != root) {
      val jParent = parents(j)
      parents(j) = root
      j = jParent
    }

    root
  }
}


object WeightedPathCompressedDisjointSet {
  /** Constructs a disjoint set with weighted trees and path compression.
   * See: Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne.
   *
   * @param indexedSet the indexed set with elements for constructing new disjoint set.
   * @tparam A type of elements in disjoint set.
   * @return a new disjoint set for provided elements.
   */
  def fromIndexedSet[A](indexedSet: IndexedSet[A]): WeightedPathCompressedDisjointSet[A] =
    new FromIndexedSet[A](indexedSet) with WeightedPathCompressedDisjointSet[A]
}

