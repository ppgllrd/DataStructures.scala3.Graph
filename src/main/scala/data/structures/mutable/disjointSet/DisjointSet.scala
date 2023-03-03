package data.structures.mutable.disjointSet

import data.structures.mutable.disjointSet.indexedSet.IndexedSet

/**
 * Trait describing a disjoint set.
 *
 * @tparam A type of elements in disjoint set.
 * @author Pepe Gallardo          
 */
trait DisjointSet[A] {
  /**
   * Total number of elements in all sets.
   */
  val size: Int

  /**
   * Number of different components in disjoint set.
   */
  def numberOfComponents: Int

  /**
   * Performs union of components corresponding to elements `x` and `y`.
   *
   * @param x one elements in first component.
   * @param y one elements in second component.
   */
  def union(x: A, y: A): Unit

  /**
   * Checks if elements `x` and `y` are in same component.
   *
   * @param x first element.
   * @param y second elements.
   * @return `true` if elements `x` and `y` are in same component.         
   */
  def areConnected(x: A, y: A): Boolean

  // one-to-one correspondence between elements and natural numbers
  protected def indexOf(x: A): Int

  protected def elementOf(i: Int): A

  protected final def validate(i: Int): Unit =
    assert(0 <= i && i < size, s"DisjointSet. element ${elementOf(i)} is not a valid one")
}


object DisjointSet {
  /**
   * Constructs a disjoint set from an indexed set (uses an interleaved path compressed implementation).
   *
   * @param indexedSet indexed set used to construct disjoint set.
   * @tparam A type of elements in disjoint set.
   * @return a new a disjoint set with elements taken from an indexed set.
   */
  def fromIndexedSet[A](indexedSet: IndexedSet[A]): DisjointSet[A] =
    InterleavedDisjointSet.fromIndexedSet(indexedSet)
}


object DisjointIntSet {
  /**
   * Constructs a disjoint set from a a set of integers (uses an interleaved path compressed implementation).
   *
   * @param size number of integers in indexed set used to construct disjoint set.
   * @return a new a disjoint set with integer elements.
   */
  def apply(size: Int): DisjointSet[Int] =
    InterleavedDisjointIntSet(size)
}