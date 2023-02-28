package data.structures.mutable.heap

import scala.reflect.ClassTag

object HashTableHeapIndexes {
  private[heap] inline val freeMark = -1
  private[heap] inline val reservedMark = -2
  private[heap] inline val noLocator = -2

  private[heap] inline val maximumLoadFactor = 0.5
}

/**
 * This hash table stores elements in heap as keys and their index within the heap (for fast lookup of such elements).
 *
 * @param initialCapacity initial capacity of hash table. Will be expanded as needed.
 * @param classTagT       a class tag for type of keys stored in hash table.
 * @tparam T Type of elements stored in heap.
 * @author Pepe Gallardo
 */
private[heap] class HashTableHeapIndexes[T](initialCapacity: Int, minHeap: MinHeap[T])(using
  classTagT: ClassTag[T]) {
  // elements in heap which are also keys in dictionary
  var keys = new Array[T](initialCapacity)
  // indexes for locating each element in heap
  var heapIndexes: Array[Int] = Array.fill[Int](initialCapacity)(HashTableHeapIndexes.freeMark)

  // indexes into array buffer of locators for a locator corresponding to each key
  var locatorIndexes: Array[Int] = Array.fill[Int](initialCapacity)(HashTableHeapIndexes.noLocator)

  // initially all cells are free
  inline def isFree(inline array: Array[Int], inline index: Int): Boolean =
    array(index) == HashTableHeapIndexes.freeMark

  inline def isFree(inline index: Int): Boolean =
    isFree(heapIndexes, index)

  // a cell is reserved if it stores an element that was extracted from heap or if it is reserved for inserting an
  // element that has already an associated locator
  inline def isReserved(inline index: Int): Boolean =
    heapIndexes(index) == HashTableHeapIndexes.reservedMark

  // a cell is occupied if it corresponds to an element in heap (heapIndexes > 0) or if it is reserved
  inline def isOccupied(inline index: Int): Boolean =
    !isFree(index)

  inline def isInHeap(inline array: Array[Int], inline index: Int): Boolean =
    array(index) >= 0

  inline def isInHeap(inline index: Int): Boolean =
    isInHeap(heapIndexes, index)

  // key with provided index has not yet a locator
  inline def noLocator(inline index: Int): Boolean =
    locatorIndexes(index) == HashTableHeapIndexes.noLocator

  // number of cells which are occupied in hash table
  private var sz = 0

  // computes hash of an element
  private def hash(key: T): Int = (key.hashCode() & 0x7fffffff) % keys.length

  // searches for index of an element in the hash table
  def indexOf(key: T): Int = {
    var index = hash(key)
    while (isOccupied(index) && keys(index) != key) {
      index = (index + 1) % keys.length
    }
    index
  }

  // computes current load factor of hash table
  protected def loadFactor: Double = sz.toDouble / keys.length

  // performs rehashing if current load factor exceeds maximum allowed one
  private[heap] def rehashing(): Boolean = {
    var performed = false
    if (loadFactor > HashTableHeapIndexes.maximumLoadFactor) {
      val oldKeys = keys
      val oldHeapIndexes = heapIndexes
      val oldLocatorIndexes = locatorIndexes
      keys = new Array[T](keys.length * 2)
      heapIndexes = Array.fill[Int](heapIndexes.length * 2)(HashTableHeapIndexes.freeMark)
      locatorIndexes = Array.fill[Int](locatorIndexes.length * 2)(HashTableHeapIndexes.noLocator)
      for (i <- oldKeys.indices) {
        if (oldHeapIndexes(i) != HashTableHeapIndexes.freeMark) {
          val index = indexOf(oldKeys(i))
          keys(index) = oldKeys(i)
          heapIndexes(index) = oldHeapIndexes(i)
          if(isInHeap(oldHeapIndexes, i))
            minHeap.hashTableIndexes(oldHeapIndexes(i)) = index
          locatorIndexes(index) = oldLocatorIndexes(i)
          minHeap.locators(locatorIndexes(index)) = index
        }
      }
      performed = true
    }
    performed
  }

  /**
   * Inserts an element and its associated index in heap in hash table. Operation is O(log n).
   *
   * @param index     index in hash table where element should be stored (according to its hash code).
   * @param key       element to insert in hash table.
   * @param heapIndex index in heap where this element is stored.
   * @return `true` if element was inserted or `false` if element was already in hash table.
   */
  def insert(index: Int, key: T, heapIndex: Int): Boolean = {
    var inserted = false
    if(isFree(index))
      sz += 1
    if (isFree(index) || isReserved(index)) {
      keys(index) = key
      heapIndexes(index) = heapIndex
      inserted = true
    }
    inserted
  }

  def insert(key: T, heapIndex: Int): (Boolean, Int) = {
    rehashing()

    val index = indexOf(key)
    var inserted = false
    if (isFree(index))
      sz += 1
    if (isFree(index) || isReserved(index)) {
      keys(index) = key
      heapIndexes(index) = heapIndex
      inserted = true
    }
    (inserted, index)
  }


  def findOrReserve(element: T): Int = {
    rehashing()
    val index = indexOf(element)
    if (isFree(index)) {
      keys(index) = element
      heapIndexes(index) = HashTableHeapIndexes.reservedMark
      sz += 1
    }
    index
  }
}
