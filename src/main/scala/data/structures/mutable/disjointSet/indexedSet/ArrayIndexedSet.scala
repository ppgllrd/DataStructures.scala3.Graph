package data.structures.mutable.disjointSet.indexedSet

object ArrayIndexedSet {
  /**
   * Constructs an indexed set with elements that are stored in an array.
   *
   * @param elements the array with elements for the indexed set.
   * @tparam A type of elements in indexed set
   * @return an indexed set with elements that are stored in an array.
   */
  def apply[A](elements: Array[A]): ArrayIndexedSet[A] =
    new ArrayIndexedSet(elements)
}


/**
 * Constructs an indexed set with elements that are stored in an array.
 *
 * @param elements the array with elements for the indexed set.
 * @tparam A type of elements in indexed set
 * @author Pepe Gallardo.          
 */
class ArrayIndexedSet[A](elements: Array[A]) extends IndexedSet[A] {
  override def size: Int = elements.length

  private val indexes = {
    val map = scala.collection.mutable.Map[A, Int]()
    for (i <- elements.indices)
      map(elements(i)) = i
    map
  }

  override def indexOf(x: A): Int = indexes(x)

  override def elementOf(i: Int): A = elements(i)
}
