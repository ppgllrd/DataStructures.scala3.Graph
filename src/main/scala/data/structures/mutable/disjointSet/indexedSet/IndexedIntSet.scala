package data.structures.mutable.disjointSet.indexedSet

object IndexedIntSet {
  def apply(size: Int): IndexedIntSet =
    new IndexedIntSet(size)
}


/**
 * An indexed set of integers in 0 .. size-1.
 *
 * @param size number of elements in set.
 */
class IndexedIntSet(val size: Int) extends IndexedSet[Int] {
  override def indexOf(x: Int): Int = x

  override def elementOf(i: Int): Int = i
}
