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
class HashTableMap[T, V](initialCapacity: Int)(using classTagT: ClassTag[T])(using classTagV: ClassTag[V])
  extends HashTable[T](initialCapacity)(using classTagT) {

  // values associated to each key in dictionary
  var values = new Array[V](initialCapacity)

  override def rehashing(): Boolean = {
    var rehashed = false
    if (loadFactor > HashTable.maximumLoadFactor) {
      val oldKeys = keys
      val oldValues = values
      val oldHeapIndexes = heapIndexes
      val oldHashTableIndexToLocators = hashTableIndexToLocators
      keys = new Array[T](keys.length * 2)
      values = new Array[V](values.length * 2)
      heapIndexes = Array.fill[Int](heapIndexes.length * 2)(HashTable.freeMark)
      hashTableIndexToLocators = Array.fill[Int](hashTableIndexToLocators.length * 2)(HashTable.noLocator)
      for (i <- oldKeys.indices) {
        if (isOccupied(oldHeapIndexes, i)) {
          val hashTableIndex = searchHashTableIndexOf(oldKeys(i))
          keys(hashTableIndex) = oldKeys(i)
          values(hashTableIndex) = oldValues(i)
          heapIndexes(hashTableIndex) = oldHeapIndexes(i)
          locatorToHashTableIndexes(oldHashTableIndexToLocators(i)) = hashTableIndex
          hashTableIndexToLocators(hashTableIndex) = oldHashTableIndexToLocators(i)
        }
      }
      rehashed = true
    }
    rehashed
  }
}
