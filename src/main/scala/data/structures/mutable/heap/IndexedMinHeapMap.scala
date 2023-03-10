package data.structures.mutable.heap

import scala.reflect.ClassTag

object IndexedMinHeapMap {
  /**
   * Constructs a new `IndexedMinHeapMap`.
   *
   * @param initialCapacity initial capacity of heap. Will be expanded as needed.
   * @param priority        `Ordering` describing priorities of elements stored in heap.
   * @param classTagT       a class tag for type of elements stored in heap.
   * @param classTagV       a class tag for type of values associated in map.
   * @tparam T Type of elements stored in heap.
   * @tparam V Type of values associated in map.
   * @return a new `IndexedMinHeapMap`.
   */
  def apply[T, V](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T])
    (using classTagV: ClassTag[V]): IndexedMinHeapMap[T, V] =
    new IndexedMinHeapMap(initialCapacity)(using priority)(using classTagT)(using classTagV)

  /**
   * Constructs a new `IndexedMinHeapMap` using default capacity (will be expanded as needed).
   *
   * @param priority  `Ordering` describing priorities of elements stored in heap.
   * @param classTagT a class tag for type of elements stored in heap.
   * @param classTagV a class tag for type of values associated in map.
   * @tparam T Type of elements stored in heap.
   * @tparam V Type of values associated in map.
   * @return a new `IndexedMinHeapMap`.
   */
  def apply[T, V](using priority: Ordering[T])(using classTagT: ClassTag[T])
    (using classTagV: ClassTag[V]): IndexedMinHeapMap[T, V] =
    new IndexedMinHeapMap(IndexedMinHeap.defaultCapacity)(using priority)(using classTagT)(using classTagV)
}

/**
 * This class implements a min-heap for a set of elements, so that each element may only be include once in the heap.
 * In order to test for equality of elements, methods `equal` and `hashCode` are used. Each element has a priority
 * (defined by `priority` parameter). This priority is used to sort elements inside the heap so that the smaller the
 * element the sooner it will be extracted from the heap. Once an element has been inserted, its priority can be
 * updated (by doing an insertion of an equal element with a different priority) and its position within the heap
 * will be adjusted accordingly.
 * Additionally this class also provides a map or dictionary from elements stored in the heap (acting as keys) to
 * values associated with them. This map is implemented by using a linear probing hash table which provides fast
 * insertions and lookups.
 * In order to locate an element in the heap (for updating its priority or for setting/retrieving its associated
 * value) a `Locator` can be used if several consecutive operations are going to be performed. This will speedup
 * those operations.
 *
 * @param initialCapacity initial capacity of heap. Will be expanded as needed.
 * @param priority        `Ordering` describing priorities of elements stored in heap.
 * @param classTagT       a class tag for type of elements stored in heap.
 * @param classTagV       a class tag for type of values associated in map.
 * @tparam T Type of elements stored in heap.
 * @tparam V Type of values associated in map.
 * @author Pepe Gallardo
 */
class IndexedMinHeapMap[T, V](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T])
  (using classTagV: ClassTag[V]) extends IndexedMinHeap[T](initialCapacity)(using priority)(using classTagT) {

  // the hash table for this heap map
  override protected val hashTable: HashTableMap[T, V] = new HashTableMap[T, V](initialCapacity * 2)

  /**
   * The map which is part of this `MinHeapMap`.
   */
  object map extends Map[T, V] {
    def update(element: T, value: V): Locator = {
      val hashTableIndex = hashTableIndexFor(element)
      hashTable.values(hashTableIndex) = value
      hashTable.locatorFor(hashTableIndex)
    }

    def update(locator: Locator, value: V): Unit = {
      val hashTableIndex = hashTableIndexFor(locator)
      hashTable.values(hashTableIndex) = value
    }

    def apply(element: T): V =
      applyIndex(hashTableIndexFor(element))

    private def applyIndex(hashTableIndex: Int): V = {
      assert(!hashTable.isFree(hashTableIndex), "apply: element is not in map")
      hashTable.values(hashTableIndex)
    }

    def apply(locator: Locator): V = {
      applyIndex(hashTableIndexFor(locator))
    }

    def get(element: T): Option[V] =
      getIndex(hashTableIndexFor(element))

    private def getIndex(hashTableIndex: Int): Option[V] = {
      if (hashTable.isFree(hashTableIndex)) {
        None
      } else if (hashTable.isReserved(hashTableIndex)) {
        val value = hashTable.values(hashTableIndex)
        Option(value)
      } else {
        Some(hashTable.values(hashTableIndex))
      }
    }

    def get(locator: Locator): Option[V] =
      getIndex(hashTableIndexFor(locator))
  }
}
