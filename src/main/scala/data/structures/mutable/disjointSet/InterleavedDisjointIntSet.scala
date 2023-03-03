/** ****************************************************************************
 * Disjoint Sets. Specialized implementation for Int elements
 *
 * Pepe Gallardo, 2019
 *
 * *************************************************************************** */

package data.structures.mutable.disjointSet

object InterleavedDisjointIntSet {
  /** Constructs a disjoint set of integers using Rem's implementation in:
   * Experiments on Union-Find Algorithms for the Disjoint-Set Data Structure.
   * Md. Mostofa Ali Patwary1, Jean Blair2, and Fredrik Manne
   *
   * @param size number of integers in new disjoint set.
   * @return a new disjoint set of integers with specified number of elements.
   */
  def apply(size: Int): InterleavedDisjointIntSet =
    new InterleavedDisjointIntSet(size)
}


/**
 * Implementation of disjoint sets as described for Rem's implementation in:
 * Experiments on Union-Find Algorithms for the Disjoint-Set Data Structure.
 * Md. Mostofa Ali Patwary1, Jean Blair2, and Fredrik Manne
 * Specialized implementation for integer elements.
 *
 * @param size number of elements in disjoint set.
 * @author Pepe Gallardo.
 */
class InterleavedDisjointIntSet(override val size: Int)
  extends InterleavedDisjointSet[Int]
    with IntElements {

  override final def areConnected(i: Int, j: Int): Boolean =
    indexAreConnected(i, j)

  override final def union(i: Int, j: Int): Unit =
    indexUnion(i, j)
}


