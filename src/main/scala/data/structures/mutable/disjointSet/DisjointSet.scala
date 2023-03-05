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
   * total number of elements in all disjoint components.
   */
  val size: Int

  /**
   * Returns number of disjoint components in disjoint set. 
   *
   * @return number of disjoint components in disjoint set.
   */
  def numberOfComponents: Int

  /**
   * Unites two disjoint components in disjoint set.
   *
   * @param x one element in first component.
   * @param y one element in second component.
   * @return `true` if elements were previously in different disjoint components.
   */
  def union(x: A, y: A): Boolean

  /**
   * Checks whether two elements are in same disjoint component.
   *
   * @param x one element.
   * @param y another element.
   * @return `true` if both elements are in same disjoint set.
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
   * Constructs a disjoint set from an indexed set (uses an with splicing interleaved implementation).
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
   * Constructs a disjoint set from a a set of integers (uses an with splicing interleaved implementation).
   *
   * @param size number of integers in indexed set used to construct disjoint set.
   * @return a new a disjoint set with integer elements.
   */
  def apply(size: Int): DisjointSet[Int] =
    InterleavedDisjointIntSet(size)
}