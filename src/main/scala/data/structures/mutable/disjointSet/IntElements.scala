package data.structures.mutable.disjointSet

/**
 * Trait for representing integer elements.
 * @author Pepe Gallardo
 */
protected trait IntElements {
  this: DisjointSet[Int] =>

  override protected final def indexOf(x: Int): Int = x

  override protected final def elementOf(i: Int): Int = i
}
