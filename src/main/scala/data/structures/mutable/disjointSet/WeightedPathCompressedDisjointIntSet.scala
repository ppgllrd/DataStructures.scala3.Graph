package data.structures.mutable.disjointSet


object WeightedPathCompressedDisjointIntSet {
  /** Constructs a disjoint set of integers with weighted trees and path compression.
   * See: Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne.
   *
   * @param size number of integers in new disjoint set.
   * @return a new disjoint set of integers with specified number of elements.
   */
  def apply(size: Int): WeightedPathCompressedDisjointIntSet =
    new WeightedPathCompressedDisjointIntSet(size)
}


/**
 * Implementation of disjoint sets with weighted trees and path compression.
 * See: Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne.
 *
 * Specialized implementation for integer elements.
 *
 * @author Pepe Gallardo
 */
class WeightedPathCompressedDisjointIntSet(override val size: Int)
  extends WeightedPathCompressedDisjointSet[Int]
    with IntElements {

  override final protected def findRoot(i: Int): Int =
    findIndexRoot(i)
}