package data.structures.mutable.disjointSet.indexedSet

/**
 * An indexed set of elements.
 *
 * @tparam A
 * @author Pepe Gallardo
 */
trait IndexedSet[A] {
  /** Total number of elements in indexed set
   *
   * @return total number of elements in indexed set.
   */
  def size: Int


  // one-to-one correspondence between elements and natural numbers:

  /**
   * Should return a different index `i` (0 <= `i` < size) for each element `x` in set.
   *
   * @param x element in the indexed set.
   * @return an unique index for element `x`.
   */
  def indexOf(x: A): Int

  /** Inverse to `indexOf`, i.e. `elementOf(indexOf(x)) == x`.
   *
   * @param i an index for an element in the indexed set.
   * @return the element in the indexed set corresponding to this index.
   */
  def elementOf(i: Int): A
}
