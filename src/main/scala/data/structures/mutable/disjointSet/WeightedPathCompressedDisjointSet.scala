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
  // a negative value means element is a root and weight of tree rooted at that node is negation of such number
  protected val parents: Array[Int] = Array.fill[Int](size)(-1)

  // number of different components
  protected var nComponents: Int = size

  def numberOfComponents: Int = nComponents

  final def areConnected(x: A, y: A): Boolean = {
    val (xRoot, xSize) = findRoot(x)
    val (yRoot, ySize) = findRoot(y)
    xRoot == yRoot
  }

  protected def findRoot(x: A): (Int, Int) =
    findIndexRoot(indexOf(x))

  protected final def findIndexRoot(i: Int): (Int, Int) = {
    validate(i)
    var root = i
    var stop = false
    while (!stop) {
      val rootParent = parents(root)
      if (rootParent < 0) // it's a root
        stop = true
      else
        root = rootParent
    }

    // path compression
    var node = i
    while (node != root) {
      val nodeParent = parents(node)
      parents(node) = root
      node = nodeParent
    }

    val weight = -parents(root)
    (root, weight)
  }

  final def union(x: A, y: A): Boolean = {
    val (xRoot, xSize) = findRoot(x)
    val (yRoot, ySize) = findRoot(y)

    if (xRoot == yRoot) {
      false
    } else {
      // link smallest tree below larger one
      // update size for new common root
      if (xSize < ySize) {
        parents(xRoot) = yRoot
        parents(yRoot) -= xSize
      } else {
        parents(yRoot) = xRoot
        parents(xRoot) -= ySize
      }
      nComponents -= 1
      true
    }
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

