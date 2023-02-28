package data.structures.mutable.heap

import scala.reflect.ClassTag

/**
 * This hash table stores elements in heap as keys, their index within the heap (for fast lookup of such elements)
 * and associated values in map.
 *
 * @param initialCapacity initial capacity of hash table. Will be expanded as needed.
 * @param classTagT       a class tag for type of keys stored in hash table.
 * @param classTagV       a class tag for type of associated values stored in hash table.
 * @tparam T Type of elements stored in heap.
 * @tparam V Type of values associated in map.
 * @author Pepe Gallardo
 */
private[heap] class HashTableHeapIndexesValues[T, V](initialCapacity: Int, minHeap: MinHeap[T])(using classTagT: ClassTag[T])(using classTagV: ClassTag[V])
  extends HashTableHeapIndexes[T](initialCapacity, minHeap)(using classTagT) {

  // values associated to each key in dictionary
  var values = new Array[V](initialCapacity)

  // performs rehashing if current load factor exceeds maximum allowed one
  private[heap] override def rehashing(): Boolean = {
    var performed = false
    if (loadFactor > HashTableHeapIndexes.maximumLoadFactor) {
      val oldKeys = keys
      val oldHeapIndexes = heapIndexes
      val oldValues = values
      val oldLocatorIndexes = locatorIndexes
      keys = new Array[T](keys.length * 2)
      heapIndexes = Array.fill[Int](heapIndexes.length * 2)(HashTableHeapIndexes.freeMark)
      values = new Array[V](values.length * 2)
      locatorIndexes = Array.fill[Int](locatorIndexes.length * 2)(HashTableHeapIndexes.noLocator)
      for (i <- oldKeys.indices) {
        if (oldHeapIndexes(i) != HashTableHeapIndexes.freeMark) {
          val index = indexOf(oldKeys(i))
          keys(index) = oldKeys(i)
          heapIndexes(index) = oldHeapIndexes(i)
          if (isInHeap(oldHeapIndexes, i))
            minHeap.hashTableIndexes(oldHeapIndexes(i)) = index
          locatorIndexes(index) = oldLocatorIndexes(i)
          minHeap.locators(locatorIndexes(index)) = index  
          values(index) = oldValues(i)
        }
      }
      performed = true
    }
    performed
  }
}
