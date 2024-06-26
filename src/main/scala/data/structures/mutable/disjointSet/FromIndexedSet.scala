package data.structures.mutable.disjointSet

import data.structures.mutable.disjointSet.indexedSet.IndexedSet

private trait FromIndexedSet[A](val indexedSet: IndexedSet[A]) {
  this: DisjointSet[A] =>

  override val size: Int =
    indexedSet.size

  override protected def indexOf(x: A): Int =
    indexedSet.indexOf(x)

  override protected def elementOf(i: Int): A =
    indexedSet.elementOf(i)
}
