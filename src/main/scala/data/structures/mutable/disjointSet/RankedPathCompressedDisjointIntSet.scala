
package data.structures.mutable.disjointSet


object RankedPathCompressedDisjointIntSet {
  /** Constructs a disjoint set of integers with ranked trees and path compression.
   *
   * @param size number of integers in new disjoint set.
   * @return a new disjoint set of integers with specified number of elements.
   */
  def apply(size: Int): RankedPathCompressedDisjointIntSet =
    new RankedPathCompressedDisjointIntSet(size)
}


/**
 * Implementation of disjoint sets with ranked trees and path compression.
 *
 * Specialized implementation for integer elements.
 *
 * @author Pepe Gallardo
 */
class RankedPathCompressedDisjointIntSet(override val size: Int)
  extends RankedPathCompressedDisjointSet[Int]
    with IntElements {

  override final protected def findRoot(i: Int): (Int, Int) =
    findIndexRoot(i)
}