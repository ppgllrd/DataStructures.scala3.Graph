package data.structures.mutable.heap

import scala.reflect.ClassTag

object MinHeapMap {
  /**
   * Constructs a new `MinHeapMap`.
   *
   * @param initialCapacity initial capacity of heap. Will be expanded as needed.
   * @param priority        `Ordering` describing priorities of elements stored in heap.
   * @param classTagT       a class tag for type of elements stored in heap.
   * @param classTagV       a class tag for type of values associated in map.
   * @tparam T Type of elements stored in heap.
   * @tparam V Type of values associated in map.
   * @return a new `MinHeapMap`.
   */
  def apply[T, V](initialCapacity: Int)(using priority: Ordering[T])(using classTagT: ClassTag[T])
    (using classTagV: ClassTag[V]): MinHeapMap[T, V] =
    new MinHeapMap(initialCapacity)(using priority)(using classTagT)(using classTagV)

  /**
   * Constructs a new `MinHeapMap` using default capacity (will be expanded as needed).
   *
   * @param priority  `Ordering` describing priorities of elements stored in heap.
   * @param classTagT a class tag for type of elements stored in heap.
   * @param classTagV a class tag for type of values associated in map.
   * @tparam T Type of elements stored in heap.
   * @tparam V Type of values associated in map.
   * @return a new `MinHeapMap`.
   */
  def apply[T, V](using priority: Ordering[T])(using classTagT: ClassTag[T])
    (using classTagV: ClassTag[V]): MinHeapMap[T, V] =
    new MinHeapMap(MinHeap.defaultCapacity)(using priority)(using classTagT)(using classTagV)
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
class MinHeapMap[T, V](initialCapacity: Int)(using priority: Ordering[T])
  (using classTagT: ClassTag[T])(using classTagV: ClassTag[V])
  extends MinHeap[T](initialCapacity)(using priority)(using classTagT) {

  // the hash table for this heap map
  override protected val hashTable: HashTableHeapIndexesValues[T, V] =
    new HashTableHeapIndexesValues[T, V](initialCapacity * 2, this)

  /**
   * The map which is part of this `MinHeapMap`.
   */
  object map extends Map[T, V] {
    private def updateIndex(hashTableIndex: Int, element: T, value: V): Locator = {
      if (hashTable.isFree(hashTableIndex)) {
        hashTable.keys(hashTableIndex) = element
        hashTable.heapIndexes(hashTableIndex) = HashTableHeapIndexes.reservedMark
      }
      hashTable.values(hashTableIndex) = value
      new Locator(hashTableIndex)
    }

    def update(element: T, value: V): Locator = {
      val hashTableIndex = hashTable.indexOf(element)
      updateIndex(hashTableIndex, element, value)
    }

    def update(locator: Locator, value: V): Unit = {
      val hashTableIndex = locator.index
      if (hashTable.isFree(hashTableIndex)) {
        hashTable.heapIndexes(hashTableIndex) = HashTableHeapIndexes.reservedMark
      }
      hashTable.values(hashTableIndex) = value
    }

    private def applyIndex(hashTableIndex: Int): V = {
      assert(!hashTable.isFree(hashTableIndex), "apply: element for locator is not in map")
      hashTable.values(hashTableIndex)
    }

    def apply(element: T): V = {
      val hashTableIndex = hashTable.indexOf(element)
      applyIndex(hashTableIndex)
    }

    def apply(locator: Locator): V = {
      applyIndex(locator.index)
    }

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

    def get(element: T): Option[V] = {
      val hashTableIndex = hashTable.indexOf(element)
      getIndex(hashTableIndex)
    }

    def get(locator: Locator): Option[V] = {
      getIndex(locator.index)
    }
  }
}

